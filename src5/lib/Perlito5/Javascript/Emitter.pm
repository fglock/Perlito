use v5;

use Perlito5::AST;

package Javascript;
{
    sub tab {
        my $level = shift;
        "    " x $level
    }

    my %safe_char = (
        '$' => 1,
        '%' => 1,
        '@' => 1,
        '&' => 1,
        '_' => 1,
        ',' => 1,
        '.' => 1,
        ':' => 1,
        ';' => 1,
        '-' => 1,
        '+' => 1,
        '*' => 1,
        ' ' => 1,
        '(' => 1,
        ')' => 1,
        '<' => 1,
        '=' => 1,
        '>' => 1,
        '[' => 1,
        ']' => 1,
        '{' => 1,
        '|' => 1,
        '}' => 1,
    );

    sub escape_string {
        my $s = shift;
        my @out;
        my $tmp = '';
        return "''" if $s eq '';
        for my $i (0 .. length($s) - 1) {
            my $c = substr($s, $i, 1);
            if  (  ($c ge 'a' && $c le 'z')
                || ($c ge 'A' && $c le 'Z')
                || ($c ge '0' && $c le '9')
                || exists( $safe_char{$c} )
                )
            {
                $tmp = $tmp . $c;
            }
            else {
                push @out, "'$tmp'" if $tmp ne '';
                push @out, "String.fromCharCode(" . ord($c) . ")";
                $tmp = '';
            }
        }
        push @out, "'$tmp'" if $tmp ne '';
        return join(' + ', @out);
    }

    sub autovivify {
        my $ast = shift;
        my $type = shift;

        my $str_init = "''";
        $str_init = '{}' if $type eq 'HASH';
        $str_init = '[]' if $type eq 'ARRAY';
        $str_init = '{}' if $type eq 'HASHREF';  # TODO use "real" reference
        $str_init = '[]' if $type eq 'ARRAYREF'; # TODO use "real" references

        if (  $ast->isa('Var') ) {
            if (  $type eq 'HASH'
               && $ast->sigil eq '$'
               )
            {
                # $a in the expression $a{'x'}
                $ast = Var->new( sigil => '%', namespace => $ast->namespace, name => $ast->name );
                my $var_js = $ast->emit_javascript;
                return [ 'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; ' ];
            }
            elsif ( $type eq 'ARRAY'
               && $ast->sigil eq '$'
               )
            {
                # $a in the expression $a[3]
                $ast = Var->new( sigil => '@', namespace => $ast->namespace, name => $ast->name );
                my $var_js = $ast->emit_javascript;
                return [ 'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; ' ];
            }
            elsif (  $type eq 'HASHREF'
               && $ast->sigil eq '$'
               )
            {
                # $a in the expression $a->{'x'}
                my $var_js = $ast->emit_javascript;
                return [ 'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; ' ];
            }
            elsif ( $type eq 'ARRAYREF'
               && $ast->sigil eq '$'
               )
            {
                # $a in the expression $a->[3]
                my $var_js = $ast->emit_javascript;
                return [ 'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; ' ];
            }
        }
        elsif ($ast->isa( 'Call' )) {
            my $var_js = $ast->emit_javascript;
            if  (  $ast->method eq 'postcircumfix:<[ ]>' ) {
                # $a->[3]
                return [ @{ autovivify( $ast->invocant, 'ARRAYREF' ) },
                         'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; '
                       ];
            }
            elsif  (  $ast->method eq 'postcircumfix:<{ }>' ) {
                # $a->{x}
                return [ @{ autovivify( $ast->invocant, 'HASHREF' ) },
                         'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; '
                       ];
            }
        }
        elsif ($ast->isa( 'Index' )) {
            my $var_js = $ast->emit_javascript;
            # $a[3][4]

            my $type;
            my $var = $ast->obj;

            if (  $var->isa('Var')
               && $var->sigil eq '$'
               )
            {
                $type = 'ARRAY';
            }
            else {
                $type = 'ARRAYREF';
            }

            return [ @{ autovivify( $ast->obj, $type ) },
                     'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; '
                   ]
        }
        elsif ($ast->isa( 'Lookup' )) {
            my $var_js = $ast->emit_javascript;
            # $a{'x'}{'y'}

            my $type;
            my $var = $ast->obj;

            if (  $var->isa('Var')
               && $var->sigil eq '$'
               )
            {
                $type = 'HASH';
            }
            else {
                $type = 'HASHREF';
            }

            return [ @{ autovivify( $var, $type ) },
                     'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; '
                   ]
        }
        return [];
    }

    sub to_str {
            my $cond = shift;
            if ($cond->isa( 'Val::Buf' )) {
                return $cond->emit_javascript;
            }
            else {
                return 'string(' . $cond->emit_javascript . ')';
            }
    }
    sub to_num {
            my $cond = shift;
            if ($cond->isa( 'Val::Int' ) || $cond->isa( 'Val::Num' )) {
                return $cond->emit_javascript;
            }
            else {
                return 'num(' . $cond->emit_javascript . ')';
            }
    }
    sub to_bool {
            my $cond = shift;
            if  (  ($cond->isa( 'Val::Int' ))
                || ($cond->isa( 'Val::Num' ))
                || ($cond->isa( 'Apply' ) && $cond->code eq 'infix:<||>')
                || ($cond->isa( 'Apply' ) && $cond->code eq 'infix:<&&>')
                || ($cond->isa( 'Apply' ) && $cond->code eq 'prefix:<!>')
                )
            {
                return $cond->emit_javascript;
            }
            else {
                return 'bool(' . $cond->emit_javascript . ')';
            }
    }

}

package Perlito5::Javascript::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{'block'} }
    sub needs_return { $_[0]->{'needs_return'} }
    sub top_level { $_[0]->{'top_level'} }

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        if ($self->{"top_level"}) {
            my $block = Perlito5::Javascript::LexicalBlock->new( block => $self->block, needs_return => $self->needs_return, top_level => 0 );
            return
                  Javascript::tab($level)   . 'try {' . "\n"
                .                               $block->emit_javascript_indented( $level + 1 ) . ';' . "\n"
                . Javascript::tab($level)   . '}' . "\n"
                . Javascript::tab($level)   . 'catch(err) {' . "\n"
                . Javascript::tab($level + 1)   . 'if ( err instanceof Error ) {' . "\n"
                . Javascript::tab($level + 2)       . 'throw(err);' . "\n"
                . Javascript::tab($level + 1)   . '}' . "\n"
                . Javascript::tab($level + 1)   . 'else {' . "\n"
                . Javascript::tab($level + 2)       . 'return(err);' . "\n"
                . Javascript::tab($level + 1)   . '}' . "\n"
                . Javascript::tab($level)   . '}';
        }

        my @block;
        for (@{$self->{"block"}}) {
            if (defined($_)) {
                push @block, $_
            }
        }
        if (!@block) {
            return Javascript::tab($level) . 'null;';
        }
        my @str;
        for my $decl ( @block ) {
            if ($decl->isa( 'Decl' ) && $decl->decl eq 'my') {
                push @str, Javascript::tab($level) . $decl->emit_javascript_init;
            }
            if ($decl->isa( 'Apply' ) && $decl->code eq 'infix:<=>') {
                my $var = $decl->arguments[0];
                if ($var->isa( 'Decl' ) && $var->decl eq 'my') {
                    push @str, Javascript::tab($level) . $var->emit_javascript_init;
                }
            }
        }
        my $last_statement;
        if ($self->{"needs_return"}) {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if (!( $decl->isa( 'Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript_indented($level) . ';';
            }
        }
        if ($self->{"needs_return"} && $last_statement) {
            if ($last_statement->isa( 'If' )) {
                my $cond      = $last_statement->cond;
                my $body      = $last_statement->body;
                my $otherwise = $last_statement->otherwise;
                if ($cond->isa( 'Var' ) && $cond->sigil eq '@') {
                    $cond = Apply->new( code => 'prefix:<@>', arguments => [ $cond ] );
                }
                $body      = Perlito5::Javascript::LexicalBlock->new( block => $body->stmts, needs_return => 1 );
                push @str, Javascript::tab($level) .
                        'if ( ' . Javascript::to_bool( $cond ) . ' ) { return (function () {' . "\n"
                        .       $body->emit_javascript_indented($level+1) . "\n"
                        . Javascript::tab($level) . '})(); }';
                if ($otherwise) {
                    $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $otherwise->stmts, needs_return => 1 );
                    push @str,
                          Javascript::tab($level) . 'else { return (function () {' . "\n"
                        .       $otherwise->emit_javascript_indented($level+1) . "\n"
                        . Javascript::tab($level) . '})(); }';
                }
            }
            elsif  $last_statement->isa( 'Apply' ) && $last_statement->code eq 'return'
                || $last_statement->isa( 'For' )
                || $last_statement->isa( 'While' )
            {
                # Return, For - no changes for now
                push @str, $last_statement->emit_javascript_indented($level)
            }
            else {
                push @str, Javascript::tab($level) . 'return(' . $last_statement->emit_javascript() . ')'
            }
        }
        return join("\n", @str) . ';';
    }
}

package CompUnit;
{
    sub emit_javascript { 
        my $self = $_[0];
        $self->emit_javascript_indented(0) 
    }
    sub emit_javascript_indented {
        my $self = $_[0];
        my $level = $_[1];

        # process 'package' statements
        my @body;
        my $i = 0;
        while ( $i <= scalar @{$self->{"body"}} ) {
            my $stmt = $self->{"body"}->[$i];
            if ( ref($stmt) eq 'Apply' && $stmt->code eq 'package' ) {
                # found an inner package
                my $name = $stmt->namespace;
                my @stmts;
                $i++;
                while (  $i <= scalar( @{$self->{"body"}} )
                      && !( ref($self->{"body"}->[$i]) eq 'Apply' && $self->{"body"}->[$i]->code eq 'package' )
                      )
                {
                    push @stmts, $self->{"body"}->[$i];
                    $i++;
                }
                push @body, CompUnit->new( name => $name, body => \@stmts );
            }
            else {
                push @body, $stmt
                    if defined $stmt;  # TODO find where undefined stmts come from
                $i++;
            }
        }

        my $class_name = Perlito5::Runtime::to_javascript_namespace($self->{"name"});
        my $str =
              '// class ' . $self->{"name"} . "\n"
            . 'if (typeof ' . $class_name . ' !== \'object\') {' . "\n"
            . '  ' . $class_name . ' = function() {};' . "\n"
            . '  ' . $class_name . ' = new ' . $class_name . ';' . "\n"
            . '  ' . $class_name . '.isa = function (s) { return s == \'' . $self->{"name"} . '\'; };' . "\n"
            . '  ' . $class_name . '._ref_ = \'' . $self->{"name"} . '\';' . "\n"
            . '}' . "\n"
            . '(function () {' . "\n"
            . '  var v__NAMESPACE = ' . $class_name . ';' . "\n";

        for my $decl ( @body ) {
            if ($decl->isa( 'Decl' ) && ( $decl->decl eq 'my' )) {
                $str = $str . '  ' . $decl->emit_javascript_init;
            }
            if ($decl->isa( 'Apply' ) && $decl->code eq 'infix:<=>') {
                my $var = $decl->arguments[0];
                if ($var->isa( 'Decl' ) && $var->decl eq 'my') {
                    $str = $str . '  ' . $var->emit_javascript_init;
                }
            }
        }
        for my $decl ( @body ) {
            if ($decl->isa( 'Sub' )) {
                $str = $str . ($decl)->emit_javascript_indented( $level + 1 ) . ";\n";
            }
        }
        for my $decl ( @body ) {
            if (  defined( $decl )
               && (!( $decl->isa( 'Decl' ) && $decl->decl eq 'my' ))
               && (!( $decl->isa( 'Sub')))
               )
            {
                $str = $str . ($decl)->emit_javascript_indented( $level + 1 ) . ";\n";
            }
        }
        $str = $str . '}'
            . ')()' . "\n";
    }
    sub emit_javascript_program {
        my $comp_units = shift;
        my $str = '';
        for my $comp_unit ( @$comp_units ) {
            $str = $str . $comp_unit->emit_javascript() . "\n";
        }
        return $str;
    }
}

package Val::Int;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift; Javascript::tab($level) . $self->{"int"} }
}

package Val::Num;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift; Javascript::tab($level) . $self->{"num"} }
}

package Val::Buf;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift; Javascript::tab($level) . Javascript::escape_string($self->{"buf"}) }
}

package Lit::Block;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $sig = 'v__';
        if ($self->{"sig"}) {
            $sig = $self->{"sig"}->emit_javascript_indented( $level + 1 );
        }
        return
              Javascript::tab($level) . "(function ($sig) \{\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"stmts"}, needs_return => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
            . Javascript::tab($level) . '})'
    }
}

package Lit::Array;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $ast = $self->expand_interpolation;
        return $ast->emit_javascript_indented( $level );
    }
}

package Lit::Hash;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $ast = $self->expand_interpolation;
        return $ast->emit_javascript_indented( $level );
    }
}

package Index;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        if (  $self->{"obj"}->isa('Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Var->new( sigil => '@', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript_indented($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
        }

        Javascript::tab($level) . $self->{"obj"}->emit_javascript() . '[' . $self->{"index_exp"}->emit_javascript() . ']';
    }
}

package Lookup;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        # my $var = $self->{"obj"}->emit_javascript;
        # return $var . '[' . $self->{"index_exp"}->emit_javascript() . ']'

        if (  $self->{"obj"}->isa('Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Var->new( sigil => '%', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript_indented($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
        }
        return $self->{"obj"}->emit_javascript_indented($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
    }
}

package Var;
{
    my $table = {
        '$' => 'v_',
        '@' => 'List_',
        '%' => 'Hash_',
        '&' => 'Code_',
    }

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        if ( $self->{"sigil"} eq '*' ) {
            my $ns = 'v__NAMESPACE';
            if ($self->{"namespace"}) {
                $ns = Perlito5::Runtime::to_javascript_namespace($self->{"namespace"});
            }
            return $ns . '["' . $self->{"name"} . '"]';
        }

        my $ns = '';
        if ($self->{"namespace"}) {
            $ns = Perlito5::Runtime::to_javascript_namespace($self->{"namespace"}) . '.';
        }
        $table->{$self->{"sigil"}} . $ns . $self->{"name"}
    }
    sub plain_name {
        my $self = shift;
        if ($self->namespace) {
            return $self->namespace . '.' . $self->name
        }
        return $self->name
    }
}

package Proto;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Javascript::tab($level) . Perlito5::Runtime::to_javascript_namespace($self->{"name"})
    }
}

package Call;
{

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $invocant = $self->{"invocant"}->emit_javascript;
        my $meth = $self->{"method"};

        # XXX Perl6
        if (  $self->{"method"} eq 'new'
           && $self->{"invocant"}->isa( 'Proto' ) 
           )
        {
            my $str = [];
            for my $field ( @{$self->{"arguments"}} ) {
                if ($field->isa('Apply') && $field->code eq 'infix:<=>>') {
                    # XXX Perl6 autogenerated constructor
                    push( @$str, '' . $field->arguments[0]->buf() . ': ' . $field->arguments[1]->emit_javascript() );
                }
                else {
                    die 'Error in constructor, field: ', $field;
                }
            }
            return
                  '(function () { '
                .   'if (' 
                .       Perlito5::Runtime::to_javascript_namespace($invocant) . '.new '
                .   ') { '
                .       'return ' . $invocant . '.new(' . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} )) . '); '
                .   '} '
                    # XXX Perl6 autogenerated constructor
                .   'var tmp = {' . join(',', @$str) . '}; '
                .   'tmp._class_ = ' . $invocant . '; '
                .   'return tmp; '
                . '})()'
        }

        if ( $self->{"method"} eq 'postcircumfix:<[ ]>' ) {
            return Javascript::tab($level) . $invocant . '[' . $self->{"arguments"}->emit_javascript() . ']'
        }
        if ( $self->{"method"} eq 'postcircumfix:<{ }>' ) {
            return Javascript::tab($level) . $invocant . '[' . $self->{"arguments"}->emit_javascript() . ']'
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            my @args = 'CallSub';
            push @args, $_->emit_javascript
                for @{$self->{"arguments"}};
            return Javascript::tab($level) . '(' . $invocant . ')(' . join(',', @args) . ')';
        }

        # try to call a method on the class; if that fails, then call a 'native js' method
        my @args = ($invocant);
        push @args, $_->emit_javascript
            for @{$self->{"arguments"}};
        return Javascript::tab($level) 
            . '(' 
            .  '('   . $invocant . '._class_ ' . '&& ' . $invocant . '._class_.' . $meth . ')' 
            . ' || ' . $invocant . '.' . $meth
            . ').call(' . join(',', @args) . ')'
    }
}

package Apply;
{

    my %op_infix_js = (
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',

        'infix:<eq>' => ' == ',
        'infix:<ne>' => ' != ',
        'infix:<le>' => ' <= ',
        'infix:<ge>' => ' >= ',

        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',
    );

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript_indented( $level );
        }

        my $code = $self->{"code"};

        if (ref $code ne '') {
            my @args = 'CallSub';
            push @args, $_->emit_javascript
                for @{$self->{"arguments"}};
            return Javascript::tab($level) . '(' . $self->{"code"}->emit_javascript() . ')(' . join(',', @args) . ')';
        }
        if ($code eq 'infix:<=>>') {
            return Javascript::tab($level) . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} ))
        }
        if (exists $op_infix_js{$code}) {
            return Javascript::tab($level) . '(' 
                . join( $op_infix_js{$code}, map( $_->emit_javascript, @{$self->{"arguments"}} ))
                . ')'
        }

        if ($code eq 'eval') {
            return
                'eval(perl5_to_js(' 
                    . Javascript::to_str($self->{"arguments"}->[0])
                . '))'
        }

        if ($code eq 'undef')      { return Javascript::tab($level) . 'null' }
        if ($code eq 'defined')    { return Javascript::tab($level) . '('  . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))    . ' != null)' }
        if ($code eq 'substr') {
            return '(' . ($self->{"arguments"}->[0])->emit_javascript()
                 . ' || "").substr(' . ($self->{"arguments"}->[1])->emit_javascript()
                 . ( defined($self->{"arguments"}->[2]) ? ', ' . ($self->{"arguments"}->[2])->emit_javascript() : '' )
                 . ')'
        }

        if ($code eq 'shift')      {
            if ( @{$self->{"arguments"}} ) {
                return 'CORE.shift(' . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} )) . ')'
            }
            return 'CORE.shift(List__)'
        }

        if ($code eq 'map') {
            my $fun  = $self->{"arguments"}->[0];
            my $list = $self->{"arguments"}->[1];
            return
                    '(function (a_) { '
                        . 'var out = []; '
                        . 'if ( a_ == null ) { return out; }; '
                        . 'for(var i = 0; i < a_.length; i++) { '
                            . 'var v__ = a_[i]; '
                            . 'out.push(' . $fun->emit_javascript . ')'
                        . '}; '
                        . 'return out;'
                    . ' })(' . $list->emit_javascript() . ')'
        }

        if ($code eq 'prefix:<!>') { return '( ' . Javascript::to_bool( $self->{"arguments"}->[0] ) . ' ? false : true)' }
        if ($code eq 'prefix:<$>') { return 'scalar' . '(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))    . ')' }
        if ($code eq 'prefix:<@>') { return '(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))    . ')' }
        if ($code eq 'prefix:<%>') { return '(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))    . ').' . 'hash' . '()' }

        if ($code eq 'prefix:<\\>') { 
            # XXX currently a no-op
            return join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} )) 
        }

        if ($code eq 'postfix:<++>') { return '('   . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return '('   . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')--' }
        if ($code eq 'prefix:<++>')  { return '++(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }
        if ($code eq 'prefix:<-->')  { return '--(' . join(' ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'infix:<x>')  { return 'str_replicate(' . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'list:<.>')
        { 
            return '('  
                . join( ' + ',
                        map( Javascript::to_str($_), @{$self->{"arguments"}} )
                      )
                . ')' 
        }

        if ($code eq 'infix:<+>')  { return 'add' . '('  . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} ))  . ')' }
        if ($code eq 'prefix:<+>') { return '('  . $self->{"arguments"}->[0]->emit_javascript()  . ')' }

        if ($code eq 'infix:<..>') {
            return '(function (a) { '
                    . 'for (var i=' . $self->{"arguments"}->[0]->emit_javascript()
                           . ', l=' . $self->{"arguments"}->[1]->emit_javascript() . '; '
                       . 'i<=l; ++i)'
                    . '{ '
                        . 'a.push(i) '
                    . '}; '
                    . 'return a '
                . '})([])'
        }

        if   $code eq 'infix:<&&>'
          || $code eq 'infix:<and>'
        {
            return 'and' . '('
                . $self->{"arguments"}->[0]->emit_javascript() . ', '
                . 'function () { return ' . $self->{"arguments"}->[1]->emit_javascript() . '; })'
        }
        if   $code eq 'infix:<||>'
          || $code eq 'infix:<or>'
        {
            return 'or' . '('
                . $self->{"arguments"}->[0]->emit_javascript() . ', '
                . 'function () { return ' . $self->{"arguments"}->[1]->emit_javascript() . '; })'
        }
        if ($code eq 'infix:<//>') { return ('defined_or') . '('
                . $self->{"arguments"}->[0]->emit_javascript() . ', '
                . 'function () { return ' . $self->{"arguments"}->[1]->emit_javascript() . '; })'
        }

        if ($code eq 'exists')     {
            my $arg = $self->{"arguments"}->[0];
            if ($arg->isa( 'Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Var->new( sigil => '%', namespace => $v->namespace, name => $v->name );
                }
                return '(' . $v->emit_javascript() . ').hasOwnProperty(' . ($arg->index_exp)->emit_javascript() . ')';
            }
            if ( $arg->isa( 'Call' ) && $arg->method eq 'postcircumfix:<{ }>' ) {
                return '(' . $arg->invocant->emit_javascript() . ').hasOwnProperty(' . $arg->arguments->emit_javascript() . ')';
            }
        }
        if ($code eq 'ternary:<?? !!>') {
            return Javascript::tab($level) 
                 . '( ' . Javascript::to_bool( $self->{"arguments"}->[0] )
                 . ' ? ' . ($self->{"arguments"}->[1])->emit_javascript()
                 . ' : ' . ($self->{"arguments"}->[2])->emit_javascript()
                 . ')'
        }
        if ($code eq 'circumfix:<( )>') {
            return Javascript::tab($level) . '(' . join(', ', map( $_->emit_javascript, @{$self->{"arguments"}} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return emit_javascript_bind( $self->{"arguments"}->[0], $self->{"arguments"}->[1], $level );
        }
        if ($code eq 'return') {
            return Javascript::tab($level) . 'throw('
                .   (@{$self->{"arguments"}} ? $self->{"arguments"}->[0]->emit_javascript() : 'null')
                . ')'
        }

        if ($self->{"namespace"}) {
            $code = Perlito5::Runtime::to_javascript_namespace($self->{"namespace"}) . '.' . ( $code );
        }
        else {
            $code = 
                  '('
                . 'v__NAMESPACE.' . $code . ' || CORE.' . $code
                . ')'
        }
        my @args = 'CallSub';
        push @args, $_->emit_javascript
            for @{$self->{"arguments"}};
        Javascript::tab($level) . $code . '(' . join(', ', @args) . ')';
    }

    sub emit_javascript_bind {
        my $parameters = shift;
        my $arguments = shift;
        my $level = shift;
        if ($parameters->isa( 'Call' )) {

            # $a->[3] = 4
            if  (  $parameters->method eq 'postcircumfix:<[ ]>' ) {
                my $str = '';
                my $var_js = $parameters->invocant->emit_javascript;
                my $auto = Javascript::autovivify( $parameters, 'ARRAYREF' );
                pop @$auto;
                $str = $str . join( '', @$auto );
                my $index_js = $parameters->arguments->emit_javascript;
                $str = $str . 'return (' . $var_js . '[' . $index_js . '] ' . ' = ' . $arguments->emit_javascript() . '); ';
                return Javascript::tab($level) . '(function () { ' . $str . '})()';
            }
 
            # $a->{x} = 4
            if  (  $parameters->method eq 'postcircumfix:<{ }>' ) {
                my $str = '';
                my $var_js = $parameters->invocant->emit_javascript;
                my $auto = Javascript::autovivify( $parameters, 'ARRAYREF' );
                pop @$auto;
                $str = $str . join( '', @$auto );
                my $index_js = $parameters->arguments->emit_javascript;
                $str = $str . 'return (' . $var_js . '[' . $index_js . '] ' . ' = ' . $arguments->emit_javascript() . '); ';
                return Javascript::tab($level) . '(function () { ' . $str . '})()';
            }

        }
        if ($parameters->isa( 'Lookup' )) {
            my $str = '';
            my $var = $parameters->obj;

            if (  $var->isa('Var')
               && $var->sigil eq '$'
               )
            {
                $var = Var->new( sigil => '%', namespace => $var->namespace, name => $var->name );
            }

            my $var_js = $var->emit_javascript;
            my $auto = Javascript::autovivify( $parameters, 'ARRAYREF' );
            pop @$auto;
            $str = $str . join( '', @$auto );
            my $index_js = $parameters->index_exp->emit_javascript;
            $str = $str . 'return (' . $var_js . '[' . $index_js . '] ' . ' = ' . $arguments->emit_javascript() . '); ';
            return Javascript::tab($level) . '(function () { ' . $str . '})()';
        }
        if ($parameters->isa( 'Index' )) {
            my $str = '';
            my $var = $parameters->obj;

            if (  $var->isa('Var')
               && $var->sigil eq '$'
               )
            {
                $var = Var->new( sigil => '@', namespace => $var->namespace, name => $var->name );
            }

            my $var_js = $var->emit_javascript;
            my $auto = Javascript::autovivify( $parameters, 'ARRAYREF' );
            pop @$auto;
            $str = $str . join( '', @$auto );
            my $index_js = $parameters->index_exp->emit_javascript;
            $str = $str . 'return (' . $var_js . '[' . $index_js . '] ' . ' = ' . $arguments->emit_javascript() . '); ';
            return Javascript::tab($level) . '(function () { ' . $str . '})()';
        }
        if      $parameters->isa( 'Var' ) && $parameters->sigil eq '@'
            ||  $parameters->isa( 'Decl' ) && $parameters->var->sigil eq '@'
        {
            $arguments = Lit::Array->new( array1 => [$arguments] );
            return Javascript::tab($level) . '(' . $parameters->emit_javascript() . ' = (' . $arguments->emit_javascript() . ').slice())';
        }
        elsif   $parameters->isa( 'Var' ) && $parameters->sigil eq '%'
            ||  $parameters->isa( 'Decl' ) && $parameters->var->sigil eq '%'
        {
            $arguments = Lit::Hash->new( hash1 => [$arguments] );
            return Javascript::tab($level) . '(' 
                . $parameters->emit_javascript() . ' = (function (_h) { '
                .   'var _tmp = {}; '
                .   'for (var _i in _h) { '
                .       '_tmp[_i] = _h[_i]; '
                .   '}; '
                .   'return _tmp; '
                . '})( ' . $arguments->emit_javascript() . '))';
        }
        Javascript::tab($level) . '(' . $parameters->emit_javascript() . ' = ' . $arguments->emit_javascript() . ')';
    }
}

package If;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{"cond"};
        if (  $cond->isa( 'Var' )
           && $cond->sigil eq '@'
           )
        {
            $cond = Apply->new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        my $s = Javascript::tab($level) . 'if ( ' . Javascript::to_bool( $cond ) . ' ) { '
            . '(function () {' . "\n"
            .       $body->emit_javascript_indented( $level + 1 ) . "\n"
            . Javascript::tab($level) . '})(); }';
        if ( $self->{"otherwise"}->stmts ) {
            my $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $self->{"otherwise"}->stmts, needs_return => 0 );
            $s = $s
                . "\n"
                . Javascript::tab($level) . 'else { '
                .   '(function () {' . "\n"
                .       $otherwise->emit_javascript_indented( $level + 1 ) . "\n"
                . Javascript::tab($level) . '})(); }';
        }
        return $s;
    }
}


package While;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        return
           Javascript::tab($level) . 'for ( '
        .  ( $self->{"init"}     ? $self->{"init"}->emit_javascript()             . '; '  : '; ' )
        .  ( $self->{"cond"}     ? Javascript::to_bool( $self->{"cond"} )                     . '; '  : '; ' )
        .  ( $self->{"continue"} ? $self->{"continue"}->emit_javascript()         . ' '   : ' '  )
        .  ') { '
            . '(function () { ' . $body->emit_javascript_indented( $level + 1 )      . ' })()'
        . ' }'
    }
}

package For;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{"cond"};
        if (!( $cond->isa( 'Var' ) && $cond->sigil eq '@' )) {
            $cond = Lit::Array->new( array1 => [$cond] )
        }
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        my $sig = 'v__';
        if ($self->{"body"}->sig()) {
            $sig = $self->{"body"}->sig->emit_javascript_indented( $level + 1 );
        }
        Javascript::tab($level) . '(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { '
            . "(function ($sig) \{ "
                . $body->emit_javascript_indented( $level + 1 )
            . ' })(a_[i_]) } })'
        . '(' . $cond->emit_javascript() . ')'
    }
}

package Decl;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Javascript::tab($level) . $self->{"var"}->emit_javascript;
    }
    sub emit_javascript_init {
        my $self = shift;
        if ($self->{"decl"} eq 'my') {
            my $str = "";
            $str = $str . 'var ' . ($self->{"var"})->emit_javascript() . ' = ';
            if ($self->{"var"})->sigil eq '%' {
                $str = $str . '{};' . "\n";
            }
            elsif ($self->{"var"})->sigil eq '@' {
                $str = $str . '[];' . "\n";
            }
            else {
                $str = $str . 'null;' . "\n";
            }
            return $str;
        }
        else {
            die "not implemented: Decl '" . $self->{"decl"} . "'";
        }
    }
}

package Sub;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

          Javascript::tab($level) . ''
        .                           ( $self->{"name"}
                                      ? 'v__NAMESPACE["' . $self->{"name"} . '"] = '
                                      : ''
                                    )
        .                           'function () {' . "\n"
        . Javascript::tab($level + 1) . 'var List__ = Array.prototype.slice.call(arguments);' . "\n"
        . Javascript::tab($level + 1) . 'if (List__[0] instanceof CallSubClass) {' . "\n"
        . Javascript::tab($level + 2) .   'List__.shift()' . "\n"
        . Javascript::tab($level + 1) . '}' . "\n"
        . Javascript::tab($level + 1) . 'else {' . "\n"
        . Javascript::tab($level + 2) .   'List__.unshift(this)' . "\n"
        . Javascript::tab($level + 1) . '}' . "\n"
        .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"block"}, needs_return => 1, top_level => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
        . Javascript::tab($level) . '}'
    }
}

package Do;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $block = $self->simplify->block;
        return
              Javascript::tab($level) . '(function () { ' . "\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
            . Javascript::tab($level) . '})()'
    }
}

package Use;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Javascript::tab($level) . '// use ' . $self->{"mod"} . "\n"
    }
}

=begin

=head1 NAME

Perlito5::Javascript::Emit - Code generator for Perlito Perl5-in-Javascript

=head1 SYNOPSIS

    $program->emit_javascript()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Javascript code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

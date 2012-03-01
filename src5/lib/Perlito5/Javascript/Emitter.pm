use v5;

use Perlito5::AST;

package Perlito5::Javascript;
{
    sub tab {
        my $level = shift;
        "\t" x $level
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

    sub to_str {
            my $cond = shift;
            if ($cond->isa( 'Perlito5::AST::Val::Buf' )) {
                return $cond->emit_javascript;
            }
            else {
                return 'string(' . $cond->emit_javascript . ')';
            }
    }
    sub to_num {
            my $cond = shift;
            if ($cond->isa( 'Perlito5::AST::Val::Int' ) || $cond->isa( 'Perlito5::AST::Val::Num' )) {
                return $cond->emit_javascript;
            }
            else {
                return 'num(' . $cond->emit_javascript . ')';
            }
    }
    sub to_bool {
            # Note: 'infix:<||>' and 'infix:<&&>' can't be optimized here, because they don't return bool
            my $cond = shift;

            if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
               && $cond->{"arguments"} && @{$cond->{"arguments"}}
               ) 
            {
                return to_bool( $cond->{"arguments"}[0] )
            }

            if  (  ($cond->isa( 'Perlito5::AST::Val::Int' ))
                || ($cond->isa( 'Perlito5::AST::Val::Num' ))
                || ($cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'prefix:<!>')
                )
            {
                return $cond->emit_javascript;
            }
            else {
                return 'bool(' . $cond->emit_javascript . ')';
            }
    }

    sub to_list {
        my $items = to_list_preprocess( $_[0] );
        @$items
        ? 'interpolate_array('
          .   join(', ', map( $_->emit_javascript, @$items ))
          . ')'
        : '[]'
    }

    sub to_list_preprocess {
        my @items;
        for my $item ( @{$_[0]} ) {
            if ($item->isa( 'Perlito5::AST::Apply' ) && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' )) {
                for my $arg ( @{ to_list_preprocess($item->arguments) } ) {
                    push( @items, $arg);
                }
            }
            elsif ($item->isa('Perlito5::AST::Apply') && $item->code eq 'infix:<=>>') {
                push @items, $item->arguments[0];
                push @items, $item->arguments[1];
            }
            else {
                push( @items, $item);
            }
        }
        return \@items;
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

        my @block;
        for (@{$self->{"block"}}) {
            if (defined($_)) {
                push @block, $_
            }
        }
        if (!@block) {
            return Perlito5::Javascript::tab($level) . 'null;';
        }
        my $out = '';
        my @str;
        my $has_local;

        for my $decl ( @block ) {
            if ($decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'local') {
                $has_local = 1;
            }
        }
        $out .= Perlito5::Javascript::tab($level) . "var local_idx = LOCAL.length;\n"
            if $has_local;
        if ($self->{"top_level"}) {
            $out .= Perlito5::Javascript::tab($level) . "try {\n";
            $level++;
        }

        my $tab = Perlito5::Javascript::tab($level);
        for my $decl ( @block ) {
            if ($decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my') {
                push @str, $decl->emit_javascript_init;
            }
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                my $var = $decl->arguments[0];
                if ($var->isa( 'Perlito5::AST::Decl' ) && $var->decl eq 'my') {
                    push @str, $var->emit_javascript_init;
                }
            }
        }
        my $last_statement;
        if ($self->{"needs_return"}) {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if (!( $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript_indented($level) . ';';
            }
        }
        if ($self->{"needs_return"} && $last_statement) {
            if ($last_statement->isa( 'Perlito5::AST::If' )) {
                my $cond      = $last_statement->cond;
                my $body      = $last_statement->body;
                my $otherwise = $last_statement->otherwise;
                $body      = Perlito5::Javascript::LexicalBlock->new( block => $body->stmts, needs_return => 1 );
                push @str,
                        'if ( ' . Perlito5::Javascript::to_bool( $cond ) . ' ) { return (function () {' . "\n"
                        .       $body->emit_javascript_indented($level+1) . "\n"
                        . Perlito5::Javascript::tab($level) . '})(); }';
                if ($otherwise) {
                    $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $otherwise->stmts, needs_return => 0 );
                    push @str,
                          Perlito5::Javascript::tab($level) . 'else { return (function () {' . "\n"
                        .       $otherwise->emit_javascript_indented($level+1) . "\n"
                        . Perlito5::Javascript::tab($level) . '})(); }';
                }
            }
            elsif (  $last_statement->isa( 'Perlito5::AST::For' )
                  || $last_statement->isa( 'Perlito5::AST::While' )
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return'
                  )
            {
                push @str, $last_statement->emit_javascript_indented($level)
            }
            else {
                if ( $has_local ) {
                    push @str, 'return cleanup_local(local_idx, (' . "\n"
                            .     $last_statement->emit_javascript_indented($level+1) . "\n"
                            .  Perlito5::Javascript::tab($level) . '));';
                }
                else {
                    push @str, 'return (' . "\n"
                            .     $last_statement->emit_javascript_indented($level+1) . "\n"
                            .  Perlito5::Javascript::tab($level) . ');';
                }
            }
        }
        if ( $has_local ) {
            push @str, 'cleanup_local(local_idx, null);';
        }
        if ($self->{"top_level"}) {
            $level--;
            return $out . join("\n", map($tab . $_, @str)) . "\n"
                . Perlito5::Javascript::tab($level)   . '}' . "\n"
                . Perlito5::Javascript::tab($level)   . 'catch(err) {' . "\n"
                . Perlito5::Javascript::tab($level + 1)   . 'if ( err instanceof Error ) {' . "\n"
                . Perlito5::Javascript::tab($level + 2)       . 'throw(err);' . "\n"
                . Perlito5::Javascript::tab($level + 1)   . '}' . "\n"
                . Perlito5::Javascript::tab($level + 1)   . 'else {' . "\n"
                . Perlito5::Javascript::tab($level + 2)
                    . ( $has_local
                      ? 'return cleanup_local(local_idx, err)'
                      : 'return(err)'
                      )
                    . ";\n"
                . Perlito5::Javascript::tab($level + 1)   . '}' . "\n"
                . Perlito5::Javascript::tab($level)   . '}';
        }
        return $out . join("\n", map($tab . $_, @str)) . ';';
    }

}

package Perlito5::AST::CompUnit;
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
            if ( ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->code eq 'package' ) {
                # found an inner package
                my $name = $stmt->namespace;
                my @stmts;
                $i++;
                while (  $i <= scalar( @{$self->{"body"}} )
                      && !( ref($self->{"body"}->[$i]) eq 'Perlito5::AST::Apply' && $self->{"body"}->[$i]->code eq 'package' )
                      )
                {
                    push @stmts, $self->{"body"}->[$i];
                    $i++;
                }
                push @body, Perlito5::AST::CompUnit->new( name => $name, body => \@stmts );
            }
            else {
                push @body, $stmt
                    if defined $stmt;  # TODO find where undefined stmts come from
                $i++;
            }
        }

        my $class_name = $self->{"name"};
        my $str = 'make_package("' . $class_name . '");' . "\n"
            . '(function () {' . "\n"
            . '  var __PACKAGE__ = "' . $class_name . '";' . "\n"
            . '  var PKG = NAMESPACE[__PACKAGE__];' . "\n";

        for my $decl ( @body ) {
            if ($decl->isa( 'Perlito5::AST::Decl' ) && ( $decl->decl eq 'my' )) {
                $str = $str . '  ' . $decl->emit_javascript_init;
            }
            if ($decl->isa( 'Perlito5::AST::Apply' ) && $decl->code eq 'infix:<=>') {
                my $var = $decl->arguments[0];
                if ($var->isa( 'Perlito5::AST::Decl' ) && $var->decl eq 'my') {
                    $str = $str . '  ' . $var->emit_javascript_init;
                }
            }
        }
        for my $decl ( @body ) {
            if ($decl->isa( 'Perlito5::AST::Sub' )) {
                $str = $str . ($decl)->emit_javascript_indented( $level + 1 ) . ";\n";
            }
        }
        for my $decl ( @body ) {
            if (  defined( $decl )
               && (!( $decl->isa( 'Perlito5::AST::Decl' ) && $decl->decl eq 'my' ))
               && (!( $decl->isa( 'Perlito5::AST::Sub')))
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

package Perlito5::AST::Val::Int;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        $self->{"int"} }
}

package Perlito5::AST::Val::Num;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        $self->{"num"} }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Perlito5::Javascript::escape_string($self->{"buf"}) }
}

package Perlito5::AST::Lit::Block;
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
              Perlito5::Javascript::tab($level) . "(function ($sig) \{\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"stmts"}, needs_return => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '})'
    }
}

package Perlito5::AST::Index;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        if (  $self->{"obj"}->isa('Perlito5::AST::Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '@', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript_indented($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
        }

          '('
        .   $self->{"obj"}->emit_javascript() 
        .   ' || (' . $self->{"obj"}->emit_javascript() . ' = new ArrayRef([]))'
        . ')._array_[' . $self->{"index_exp"}->emit_javascript() . ']';
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        # my $var = $self->{"obj"}->emit_javascript;
        # return $var . '[' . $self->{"index_exp"}->emit_javascript() . ']'

        if (  $self->{"obj"}->isa('Perlito5::AST::Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Perlito5::AST::Var->new( sigil => '%', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_javascript_indented($level) . '[' . $self->{"index_exp"}->emit_javascript() . ']';
        }

          '('
        .   $self->{"obj"}->emit_javascript() 
        .   ' || (' . $self->{"obj"}->emit_javascript() . ' = new HashRef({}))'
        . ')._hash_[' . $self->{"index_exp"}->emit_javascript() . ']';
    }
}

package Perlito5::AST::Var;
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
            my $ns = 'PKG';
            if ($self->{"namespace"}) {
                $ns = 'NAMESPACE["' . $self->{"namespace"} . '"]';
            }
            return $ns . '["' . $self->{"name"} . '"]';
        }

        my $ns = '';
        if ($self->{"namespace"}) {
            $ns = 'NAMESPACE["' . $self->{"namespace"} . '"].';
        }
        $ns . $table->{$self->{"sigil"}} . $self->{"name"}
    }
}

package Perlito5::AST::Proto;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        'CLASS["' . $self->{"name"} . '"]'
    }
}

package Perlito5::AST::Call;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $invocant = $self->{"invocant"}->emit_javascript;
        my $meth = $self->{"method"};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            return 
                  '('
                .   $invocant 
                .   ' || (' . $invocant . ' = new ArrayRef([]))'
                . ')._array_[' . $self->{"arguments"}->emit_javascript() . ']';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            return
                  '('
                .   $invocant 
                .   ' || (' . $invocant . ' = new HashRef({}))'
                . ')._hash_[' . $self->{"arguments"}->emit_javascript() . ']';
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            my @args = ();
            push @args, $_->emit_javascript
                for @{$self->{"arguments"}};
            return '(' . $invocant . ')([' . join(',', @args) . '])';
        }
        my @args = ($invocant);
        push @args, $_->emit_javascript
            for @{$self->{"arguments"}};
        return $invocant . '._class_.' . $meth . '([' . join(',', @args) . '])'
    }
}

package Perlito5::AST::Apply;
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
            my @args = ();
            push @args, $_->emit_javascript
                for @{$self->{"arguments"}};
            return '(' . $self->{"code"}->emit_javascript_indented( $level ) . ')(' . join(',', @args) . ')';
        }
        if ($code eq 'infix:<=>>') {
            return join(', ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} ))
        }
        if (exists $op_infix_js{$code}) {
            return '(' 
                . join( $op_infix_js{$code}, map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} ))
                . ')'
        }

        if ($code eq 'eval') {
            return
                'eval(perl5_to_js(' 
                    . Perlito5::Javascript::to_str($self->{"arguments"}->[0])
                . '))'
        }

        if ($code eq 'undef')      {
            if ( $self->{"arguments"} && @{$self->{"arguments"}} ) {
                return '(' . $self->{"arguments"}->[0]->emit_javascript . ' = null)'
            }
            return 'null'
        }

        if ($code eq 'defined')    { return '('  . join(' ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} ))    . ' != null)' }

        if ($code eq 'shift')      {
            if ( $self->{"arguments"} && @{$self->{"arguments"}} ) {
                return 'PKG.shift([' . join(', ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} )) . '])'
            }
            return 'PKG.shift([List__])'
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
                            . 'out.push(' . $fun->emit_javascript_indented( $level ) . ')'
                        . '}; '
                        . 'return out;'
                    . ' })(' . $list->emit_javascript() . ')'
        }

        if ( $code eq 'prefix:<!>' ) {
            return '!( ' . Perlito5::Javascript::to_bool( $self->{"arguments"}->[0] ) . ')';
        }

        if ( $code eq 'prefix:<$>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '(' . $arg->emit_javascript . ')._scalar_';
        }
        if ( $code eq 'prefix:<@>' ) {
            my $arg = $self->{"arguments"}->[0];
            return
                  '('
                .   $arg->emit_javascript_indented( $level ) 
                .   ' || (' . $arg->emit_javascript_indented( $level ) . ' = new ArrayRef([]))'
                . ')._array_';
        }
        if ( $code eq 'prefix:<%>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '(' . $arg->emit_javascript_indented( $level ) . ')._hash_';
        }

        if ( $code eq 'circumfix:<[ ]>' ) {
            return '(new ArrayRef(' . Perlito5::Javascript::to_list($self->{"arguments"}) . '))'
        }
        if ( $code eq 'circumfix:<{ }>' ) {
            return '(new HashRef(array_to_hash(' . Perlito5::Javascript::to_list($self->{"arguments"}) . ')))'
        }
        if ( $code eq 'prefix:<\\>' ) {
            my $arg = $self->{"arguments"}->[0];
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return '(new ArrayRef(' . $arg->emit_javascript_indented( $level ) . '))';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new HashRef(' . $arg->emit_javascript_indented( $level ) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ($arg->{"namespace"}) {
                        return 'NAMESPACE["' . $arg->{"namespace"} . '"].' . $arg->{"name"};
                    }
                    else {
                        return 'PKG.' . $arg->{"name"};
                    }
                }
            }
            return '(new ScalarRef(' . $arg->emit_javascript_indented( $level ) . '))';
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
                        map( Perlito5::Javascript::to_str($_), @{$self->{"arguments"}} )
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

        if ($code eq 'exists') {
            my $arg = $self->{"arguments"}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    $v = Perlito5::AST::Var->new( sigil => '%', namespace => $v->namespace, name => $v->name );
                    return '(' . $v->emit_javascript() . ').hasOwnProperty(' . ($arg->index_exp)->emit_javascript() . ')';
                }
                return '(' . $v->emit_javascript() . ')._hash_.hasOwnProperty(' . ($arg->index_exp)->emit_javascript() . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return '(' . $arg->invocant->emit_javascript() . ')._hash_.hasOwnProperty(' . $arg->{"arguments"}->emit_javascript() . ')';
                }
            }
        }
        if ($code eq 'ternary:<?? !!>') {
            return
                   '( ' . Perlito5::Javascript::to_bool( $self->{"arguments"}->[0] )
                 . ' ? ' . ($self->{"arguments"}->[1])->emit_javascript()
                 . ' : ' . ($self->{"arguments"}->[2])->emit_javascript()
                 . ')'
        }
        if ($code eq 'circumfix:<( )>') {
            return '(' . join(', ', map( $_->emit_javascript_indented( $level ), @{$self->{"arguments"}} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return emit_javascript_bind( $self->{"arguments"}->[0], $self->{"arguments"}->[1], $level );
        }
        if ($code eq 'return') {
            return 'throw('
                .   ( $self->{"arguments"} && @{$self->{"arguments"}} 
                    ? $self->{"arguments"}->[0]->emit_javascript() 
                    : 'null'
                    )
                . ')'
        }

        if ($self->{"namespace"}) {

            if (  $self->{"namespace"} eq 'JS' 
               && $code eq 'inline'
               ) 
            {
                if ( $self->{"arguments"}->[0]->isa('Perlito5::AST::Val::Buf') ) {
                    # JS::inline('var x = 123')
                    return $self->{"arguments"}[0]{"buf"};
                }
                else {
                    die "JS::inline needs a string constant";
                }
            }

            $code = 'NAMESPACE["' . $self->{"namespace"} . '"].' . $code;
        }
        else {
            $code = 'PKG.' . $code
        }
        my @args = ();
        push @args, $_->emit_javascript_indented( $level )
            for @{$self->{"arguments"}};
        $code . '([' . join(', ', @args) . '])';
    }

    sub emit_javascript_bind {
        my $parameters = shift;
        my $arguments = shift;
        my $level = shift;

        if      $parameters->isa( 'Perlito5::AST::Var' ) && $parameters->sigil eq '@'
            ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '@'
        {
            return '(' . $parameters->emit_javascript() . ' = ' . Perlito5::Javascript::to_list([$arguments]) . ')'
        }
        elsif   $parameters->isa( 'Perlito5::AST::Var' ) && $parameters->sigil eq '%'
            ||  $parameters->isa( 'Perlito5::AST::Decl' ) && $parameters->var->sigil eq '%'
        {
            return '(' . $parameters->emit_javascript() . ' = array_to_hash(' . Perlito5::Javascript::to_list([$arguments]) . '))' 
        }
        '(' . $parameters->emit_javascript_indented( $level ) . ' = ' . $arguments->emit_javascript_indented( $level ) . ')';
    }
}

package Perlito5::AST::If;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{"cond"};
        my $body  = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        my $s = 'if ( ' . Perlito5::Javascript::to_bool( $cond ) . ' ) { '
            . '(function () {' . "\n"
            .       $body->emit_javascript_indented( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '})(); }';
        if ( @{ $self->{"otherwise"}->stmts } ) {
            my $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $self->{"otherwise"}->stmts, needs_return => 0 );
            $s = $s
                . "\n"
                . Perlito5::Javascript::tab($level) . 'else { '
                .   '(function () {' . "\n"
                .       $otherwise->emit_javascript_indented( $level + 1 ) . "\n"
                . Perlito5::Javascript::tab($level) . '})(); }';
        }
        return $s;
    }
}


package Perlito5::AST::While;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        return
           'for ( '
        .  ( $self->{"init"}     ? $self->{"init"}->emit_javascript()           . '; '  : '; ' )
        .  ( $self->{"cond"}     ? Perlito5::Javascript::to_bool( $self->{"cond"} )       . '; '  : '; ' )
        .  ( $self->{"continue"} ? $self->{"continue"}->emit_javascript()       . ' '   : ' '  )
        .  ') { '
            . '(function () {' . "\n" . $body->emit_javascript_indented( $level + 1 )      . ' })()'
        . ' }'
    }
}

package Perlito5::AST::For;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $cond = Perlito5::Javascript::to_list([$self->{"cond"}]);
        my $body = Perlito5::Javascript::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        my $sig  = 'v__';
        if ($self->{"body"}->sig()) {
            $sig = $self->{"body"}->sig->emit_javascript_indented( $level + 1 );
        }
        '(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { '
            . "(function ($sig) {\n"
                . $body->emit_javascript_indented( $level + 1 )
            . ' })(a_[i_]) } })'
        . '(' . $cond . ')'
    }
}

package Perlito5::AST::Decl;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        if ($self->{"decl"} eq 'local') {

            # TODO - add grammar support
            # if ($self->var->isa("Lookup")) {
            #     return 
            #         'set_local(' . $self->var->{"obj"}->emit_javascript() . ', '
            #                      . $self->var->{"index_exp"}->emit_javascript() . ', '
            #                      . '""); '
            #         . $self->{"var"}->emit_javascript_indented( $level );
            # }

            my $ns = 'PKG';
            if ($self->{"var"}{"namespace"}) {
                $ns = 'NAMESPACE["' . $self->{"var"}{"namespace"} . '"]';
            }

            return
                  'set_local(' . $ns . ','
                               . Perlito5::Javascript::escape_string($self->{"var"}{"name"}) . ','
                               . Perlito5::Javascript::escape_string($self->{"var"}{"sigil"}) . '); ' 
                . $self->{"var"}->emit_javascript_indented( $level );
        }
        $self->{"var"}->emit_javascript_indented( $level );
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
            die "not implemented: Perlito5::AST::Decl '" . $self->{"decl"} . "'";
        }
    }
}

package Perlito5::AST::Sub;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $s =                     'function (List__) {' . "\n"
        .   (Perlito5::Javascript::LexicalBlock->new( block => $self->{"block"}, needs_return => 1, top_level => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
        . Perlito5::Javascript::tab($level) . '}';

        ( $self->{"name"}
          ? 'make_sub(__PACKAGE__, "' . $self->{"name"} . '", ' . $s . ')'
          : $s
        )

    }
}

package Perlito5::AST::Do;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $block = $self->simplify->block;
        return
              '(function () {' . "\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
            . Perlito5::Javascript::tab($level) . '})()'
    }
}

package Perlito5::AST::Use;
{
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        '// use ' . $self->{"mod"} . "\n"
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

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

use v5;

use Perlito5::AST;

class Javascript {
    sub tab {
        my $level = shift;
        "    " x $level
    }

    my %safe_char = (
        '_' => 1,
        ',' => 1,
        '.' => 1,
        ':' => 1,
        '-' => 1,
        '+' => 1,
        '*' => 1,
        ' ' => 1,
        '(' => 1,
        ')' => 1,
        '<' => 1,
        '>' => 1,
        '[' => 1,
        ']' => 1,
    );

    sub escape_string {
        my $s = shift;
        my @out;
        my $tmp = '';
        return "''" if $s eq '';
        for my $i (0 .. length($s) - 1) {
            my $c = substr($s, $i, 1);
            if  (  (($c ge 'a') && ($c le 'z'))
                || (($c ge 'A') && ($c le 'Z'))
                || (($c ge '0') && ($c le '9'))
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
                $ast = Var->new( sigil => '%', twigil => $ast->twigil, namespace => $ast->namespace, name => $ast->name );
                my $var_js = $ast->emit_javascript;
                return [ 'if (' . $var_js . ' == null) { ' . $var_js . ' = ' . $str_init . ' }; ' ];
            }
            elsif ( $type eq 'ARRAY'
               && $ast->sigil eq '$'
               )
            {
                # $a in the expression $a[3]
                $ast = Var->new( sigil => '@', twigil => $ast->twigil, namespace => $ast->namespace, name => $ast->name );
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

}

class Perlito5::Javascript::LexicalBlock {
    has $.block;
    has $.needs_return;
    has $.top_level;
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        if ($.top_level) {
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
        for (@{$.block}) {
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
        if ($.needs_return) {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if (!( $decl->isa( 'Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_javascript_indented($level) . ';';
            }
        }
        if ($.needs_return && $last_statement) {
            if ($last_statement->isa( 'If' )) {
                my $cond      = $last_statement->cond;
                my $body      = $last_statement->body;
                my $otherwise = $last_statement->otherwise;
                if ($cond->isa( 'Var' ) && $cond->sigil eq '@') {
                    $cond = Apply->new( code => 'prefix:<@>', arguments => [ $cond ] );
                }
                $body      = Perlito5::Javascript::LexicalBlock->new( block => $body->stmts, needs_return => 1 );
                push @str, Javascript::tab($level) .
                        'if ( ' . ('bool') . '(' . $cond->emit_javascript() . ') ) { return (function () {' . "\n"
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

class CompUnit {
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
        while ( $i <= scalar @{$.body} ) {
            my $stmt = $.body->[$i];
            if ( $stmt->isa( 'Apply' ) && $stmt->code eq 'package' ) {
                # found an inner package
                my $name = $stmt->namespace;
                my @stmts;
                $i++;
                while (  $i <= scalar( @{$.body} )
                      && !( $.body->[$i]->isa( 'Apply' ) && $.body->[$i]->code eq 'package' )
                      )
                {
                    push @stmts, $.body->[$i];
                    $i++;
                }
                push @body, CompUnit->new( name => $name, body => \@stmts );
            }
            else {
                push @body, $stmt;
                $i++;
            }
        }

        my $class_name = Main::to_javascript_namespace($.name);
        my $str =
              '// class ' . $.name . "\n"
            . 'if (typeof ' . $class_name . ' !== \'object\') {' . "\n"
            . '  ' . $class_name . ' = function() {};' . "\n"
            . '  ' . $class_name . ' = new ' . $class_name . ';' . "\n"
            . '  ' . $class_name . '.' . ('isa') . ' = function (s) { return s == \'' . $.name . '\'; };' . "\n"
            . '  ' . $class_name . '.' . ('ref') . ' = function (s) { return \'' . $.name . '\'; };' . "\n"
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
            if ($decl->isa( 'Decl' ) && ( $decl->decl eq 'has' )) {
                $str = $str
              . '  // accessor ' . $decl->var->name() . "\n"
              . '  ' . $class_name . '.v_' . $decl->var->name() . ' = null;' . "\n"
              . '  ' . $class_name . '.' . ( $decl->var->name() )
                    . ' = function () { return this.v_' . $decl->var->name() . '; };' . "\n";
            }
            if ($decl->isa( 'Sub' )) {
                my $sig      = $decl->sig;
                my $pos      = $sig->positional;
                my $block    = Perlito5::Javascript::LexicalBlock->new( block => $decl->block, needs_return => 1, top_level => 1 );
                $str = $str
              . '  // sub ' . $decl->name() . "\n"
              . '  ' . $class_name . '.' . ( $decl->name() )
                    . ' = function (' . join(', ', map( $_->emit_javascript(), @$pos )) . ') {' . "\n"
                # create @_
              . Javascript::tab($level + 1) . 'var List__ = Array.prototype.slice.call(arguments);' . "\n"
              . Javascript::tab($level + 1) . 'if (List__[0] instanceof CallSubClass) {' . "\n"
              . Javascript::tab($level + 2) .   'List__.shift()' . "\n"
              . Javascript::tab($level + 1) . '}' . "\n"
              . Javascript::tab($level + 1) . 'else {' . "\n"
              . Javascript::tab($level + 2) .   'List__.unshift(this)' . "\n"
              . Javascript::tab($level + 1) . '}' . "\n"

              .         $block->emit_javascript_indented( $level + 1 ) . "\n"
              . '  }' . "\n"
              . '  ' . $class_name . '.' . ( $decl->name() ) . ';  // v8 bug workaround' . "\n";
            }
        }
        for my $decl ( @body ) {
            if (  defined( $decl )
               && (!( $decl->isa( 'Decl' ) && (( $decl->decl eq 'has' ) || ( $decl->decl eq 'my' )) ))
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

class Val::Int {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift; Javascript::tab($level) . $.int }
}

class Val::Bit {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift; Javascript::tab($level) . ($.bit ? 'true' : 'false') }
}

class Val::Num {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift; Javascript::tab($level) . $.num }
}

class Val::Buf {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift; Javascript::tab($level) . Javascript::escape_string($.buf) }
}

class Lit::Block {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $sig = 'v__';
        if ($.sig) {
            $sig = $.sig->emit_javascript_indented( $level + 1 );
        }
        return
              Javascript::tab($level) . "(function ($sig) \{\n"
            .   (Perlito5::Javascript::LexicalBlock->new( block => $.stmts, needs_return => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
            . Javascript::tab($level) . '})'
    }
}

class Lit::Array {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $ast = $self->expand_interpolation;
        return $ast->emit_javascript_indented( $level );
    }
}

class Lit::Hash {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $ast = $self->expand_interpolation;
        return $ast->emit_javascript_indented( $level );
    }
}

class Index {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        if (  $.obj->isa('Var')
           && $.obj->sigil eq '$'
           )
        {
            my $v = Var->new( sigil => '@', twigil => $.obj->twigil, namespace => $.obj->namespace, name => $.obj->name );
            return $v->emit_javascript_indented($level) . '[' . $.index_exp->emit_javascript() . ']';
        }

        Javascript::tab($level) . $.obj->emit_javascript() . '[' . $.index_exp->emit_javascript() . ']';
    }
}

class Lookup {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        # my $var = $.obj->emit_javascript;
        # return $var . '[' . $.index_exp->emit_javascript() . ']'

        if (  $.obj->isa('Var')
           && $.obj->sigil eq '$'
           && $.obj->name ne 'MATCH'  # XXX $MATCH is the Perl6 match object
           )
        {
            my $v = Var->new( sigil => '%', twigil => $.obj->twigil, namespace => $.obj->namespace, name => $.obj->name );
            return $v->emit_javascript_indented($level) . '[' . $.index_exp->emit_javascript() . ']';
        }
        return $.obj->emit_javascript_indented($level) . '[' . $.index_exp->emit_javascript() . ']';
    }
}

class Var {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $table = {
            '$' => 'v_',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        }
        my $ns = '';
        if ($.namespace) {
            $ns = Main::to_javascript_namespace($.namespace) . '.';
        }
           ( $.twigil eq '.' )
        ?  ( 'v_self.v_' . $.name . '' )
        :  ( $table->{$.sigil} . $ns . $.name )
    }
    sub plain_name {
        my $self = shift;
        if ($self->namespace) {
            return $self->namespace . '.' . $self->name
        }
        return $self->name
    }
}

class Proto {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Javascript::tab($level) . Main::to_javascript_namespace($.name)
    }
}

class Call {

    my %method_js = (
        'isa'    => 'isa',
    );

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $invocant = $.invocant->emit_javascript;

        # XXX Perl6
        if ($.method eq 'new') {
            my $str = [];
            for my $field ( @{$.arguments} ) {
                if ($field->isa('Apply') && $field->code eq 'infix:<=>>') {
                    push( @$str, 'v_' . $field->arguments[0]->buf() . ': ' . $field->arguments[1]->emit_javascript() );
                }
                else {
                    die 'Error in constructor, field: ', $field;
                }
            }
            return
                  '(function () { '
                .   'if (' . Main::to_javascript_namespace($invocant) . '.hasOwnProperty("new") ) { '
                .       'return ' . $invocant . '.new(' . join(', ', map( $_->emit_javascript, @{$.arguments} )) . '); '
                .   '} '
                    # XXX Perl6 autogenerated constructor
                .   'var tmp = {' . join(',', @$str) . '}; '
                .   'tmp.__proto__ = ' . Main::to_javascript_namespace($invocant) . '; '
                .   'return tmp; '
                . '})()'
        }

        if (exists( %method_js{ $.method } )) {
            return ( $.method ) . '('
                    . $invocant
                    . ( @{$.arguments} ? ', ' . join(', ', map( $_->emit_javascript, @{$.arguments} )) : '' )
                . ')';
        }

        my $meth = $.method;

        if ( $.method eq 'postcircumfix:<[ ]>' ) {
            return Javascript::tab($level) . $invocant . '[' . $.arguments->emit_javascript() . ']'
        }
        if ( $.method eq 'postcircumfix:<{ }>' ) {
            return Javascript::tab($level) . $invocant . '[' . $.arguments->emit_javascript() . ']'
        }

        if  ($meth eq 'postcircumfix:<( )>')  {
            return '(' . $invocant . ')(' . join(', ', map( $_->emit_javascript, @{$.arguments} )) . ')';
        }

        # try to call a method on the prototype; if that fails, then call a method on $self.
        # this prevents properties from overriding methods with the same name
        my @args = ($invocant);
        push @args, $_->emit_javascript
            for @{$.arguments};
        return Javascript::tab($level) 
            . '(' 
            .          'typeof(' . $invocant . '.__proto__) != \'undefined\' '
            .   '&& ' . $invocant . '.__proto__.hasOwnProperty("' . ( $meth ) . '") '
            . '? ' . $invocant . '.__proto__.' . ( $meth ) . '.call(' . join(',', @args) . ') '
            . ': ' . $invocant . '.' . ( $meth ) . '(' . join(', ', map( $_->emit_javascript, @{$.arguments} )) . ')'
            . ')';
    }
}

class Apply {

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

    my %op_global_js = (
        'index'   => 1,
        'die'     => 1,
        'unshift' => 1,
        'push'    => 1,
        'pop'     => 1,
        'join'    => 1,
        'chr'     => 1,
        # 'say'     => 1,
        'bless'   => 1,
        # 'print'   => 1,
        'warn'    => 1,
        'scalar'  => 1,
        'keys'    => 1,
        'values'  => 1,
        'length'  => 1,
        'ref'     => 1,
    );

    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript_indented( $level );
        }

        my $code = $.code;

        if (ref $code ne '') {
            return Javascript::tab($level) . '(' . $.code->emit_javascript() . ')->(' . join(',', map( $_->emit(), @{$.arguments})) . ')';
        }
        if ($code eq 'infix:<=>>') {
            return Javascript::tab($level) . join(', ', map( $_->emit_javascript, @{$.arguments} ))
        }
        if (exists $op_infix_js{$code}) {
            return Javascript::tab($level) . '(' 
                . join( $op_infix_js{$code}, map( $_->emit_javascript, @{$.arguments} ))
                . ')'
        }

        if ($code eq 'eval') {
            return
                'eval(perl5_to_js(' 
                    . ('string') . '(' . ($.arguments->[0])->emit_javascript() . ')'
                . '))'
        }

        if ($code eq 'undef')      { return Javascript::tab($level) . 'null' }
        if ($code eq 'defined')    { return Javascript::tab($level) . '('  . join(' ', map( $_->emit_javascript, @{$.arguments} ))    . ' != null)' }
        if ($code eq 'substr') {
            return '(' . ($.arguments->[0])->emit_javascript()
                 . ' || "").substr(' . ($.arguments->[1])->emit_javascript()
                 . ( defined($.arguments->[2]) ? ', ' . ($.arguments->[2])->emit_javascript() : '' )
                 . ')'
        }

        if ($code eq 'shift')      {
            if ( @{$.arguments} ) {
                return 'CORE.shift(' . join(', ', map( $_->emit_javascript, @{$.arguments} )) . ')'
            }
            return 'CORE.shift(List__)'
        }

        if ($code eq 'map') {
            my $fun  = $.arguments->[0];
            my $list = $.arguments->[1];
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

        if ($code eq 'chr')        { return 'String.fromCharCode(' . ('num') . '(' . ($.arguments->[0])->emit_javascript() . '))' }
        if ($code eq 'ord')        { return '(' . ($.arguments->[0])->emit_javascript() . ').charCodeAt(0)' }

        if ($code eq 'Int')        { return 'parseInt(' . ($.arguments->[0])->emit_javascript() . ')' }
        if ($code eq 'Num')        { return 'parseFloat(' . ($.arguments->[0])->emit_javascript() . ')' }

        if ($code eq 'prefix:<!>') { return '( ' . ('bool') . '(' . join(' ', map( $_->emit_javascript, @{$.arguments} ))    . ') ? false : true)' }
        if ($code eq 'prefix:<?>') { return '( ' . ('bool') . '(' . join(' ', map( $_->emit_javascript, @{$.arguments} ))    . ') ? true : false)' }
        if ($code eq 'prefix:<$>') { return ('scalar') . '(' . join(' ', map( $_->emit_javascript, @{$.arguments} ))    . ')' }
        if ($code eq 'prefix:<@>') { return '(' . join(' ', map( $_->emit_javascript, @{$.arguments} ))    . ')' }
        if ($code eq 'prefix:<%>') { return '(' . join(' ', map( $_->emit_javascript, @{$.arguments} ))    . ').' . ('hash') . '()' }

        if ($code eq 'prefix:<\\>') { 
            # XXX currently a no-op
            return join(' ', map( $_->emit_javascript, @{$.arguments} )) 
        }

        if ($code eq 'postfix:<++>') { return '('   . join(' ', map( $_->emit_javascript, @{$.arguments} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return '('   . join(' ', map( $_->emit_javascript, @{$.arguments} ))  . ')--' }
        if ($code eq 'prefix:<++>')  { return '++(' . join(' ', map( $_->emit_javascript, @{$.arguments} ))  . ')' }
        if ($code eq 'prefix:<-->')  { return '--(' . join(' ', map( $_->emit_javascript, @{$.arguments} ))  . ')' }

        if ($code eq 'infix:<x>')  { return 'str_replicate(' . join(', ', map( $_->emit_javascript, @{$.arguments} ))  . ')' }

        if ($code eq 'list:<.>')
        { 
            return '(' . ('string') . '('
                . join( ') + ' . ('string') . '(',
                        map( $_->emit_javascript, @{$.arguments} )
                      )
                . '))' 
        }

        if ($code eq 'infix:<+>')  { return ('add') . '('  . join(', ', map( $_->emit_javascript, @{$.arguments} ))  . ')' }
        if ($code eq 'prefix:<+>') { return '('  . $.arguments->[0]->emit_javascript()  . ')' }

        if ($code eq 'infix:<..>') {
            return '(function (a) { '
                    . 'for (var i=' . $.arguments->[0]->emit_javascript()
                           . ', l=' . $.arguments->[1]->emit_javascript() . '; '
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
            return ('and') . '('
                . $.arguments->[0]->emit_javascript() . ', '
                . 'function () { return ' . $.arguments->[1]->emit_javascript() . '; })'
        }
        if   $code eq 'infix:<||>'
          || $code eq 'infix:<or>'
        {
            return ('or') . '('
                . $.arguments->[0]->emit_javascript() . ', '
                . 'function () { return ' . $.arguments->[1]->emit_javascript() . '; })'
        }
        if ($code eq 'infix:<//>') { return ('defined_or') . '('
                . $.arguments->[0]->emit_javascript() . ', '
                . 'function () { return ' . $.arguments->[1]->emit_javascript() . '; })'
        }

        if ($code eq 'exists')     {
            my $arg = $.arguments->[0];
            if ($arg->isa( 'Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Var')
                   && $v->sigil eq '$'
                   && $v->name ne 'MATCH'  # XXX $MATCH is the Perl6 match object
                   )
                {
                    $v = Var->new( sigil => '%', twigil => $v->twigil, namespace => $v->namespace, name => $v->name );
                }
                return '(' . $v->emit_javascript() . ').hasOwnProperty(' . ($arg->index_exp)->emit_javascript() . ')';
            }
            if ( $arg->isa( 'Call' ) && $arg->method eq 'postcircumfix:<{ }>' ) {
                return '(' . $arg->invocant->emit_javascript() . ').hasOwnProperty(' . $arg->arguments->emit_javascript() . ')';
            }
        }
        if ($code eq 'ternary:<?? !!>') {
            return Javascript::tab($level) 
                 . '( ' . ('bool') . '(' . ($.arguments->[0])->emit_javascript() . ')'
                 . ' ? ' . ($.arguments->[1])->emit_javascript()
                 . ' : ' . ($.arguments->[2])->emit_javascript()
                 . ')'
        }
        if ($code eq 'circumfix:<( )>') {
            return Javascript::tab($level) . '(' . join(', ', map( $_->emit_javascript, @{$.arguments} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return emit_javascript_bind( $.arguments->[0], $.arguments->[1], $level );
        }
        if ($code eq 'return') {
            return Javascript::tab($level) . 'throw('
                .   (@{$.arguments} ? $.arguments->[0]->emit_javascript() : 'null')
                . ')'
        }

        if ($.namespace) {
            $code = Main::to_javascript_namespace($.namespace) . '.' . ( $code );
        }
        elsif (!exists( $op_global_js{$code} )) {
            $code = 
                  '(v__NAMESPACE.hasOwnProperty("' . ( $code ) . '") '
                . '? v__NAMESPACE.' . ( $code ) . ' '
                . ': CORE.' . ( $code )
                . ')'
        }
        else {
            # XXX Obsolete - %op_global_js should go away
            $code = ( $.code );
            return Javascript::tab($level) . $code . '(' . join(', ', map( $_->emit_javascript, @{$.arguments} )) . ')';
        }
        my @args = 'CallSub';
        push @args, $_->emit_javascript
            for @{$.arguments};
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

            # $var->attr = 3;
            return Javascript::tab($level) . '(' . ($parameters->invocant)->emit_javascript() . '.v_' . $parameters->method() . ' = ' . $arguments->emit_javascript() . ')';
        }
        if ($parameters->isa( 'Lookup' )) {
            my $str = '';
            my $var = $parameters->obj;

            if (  $var->isa('Var')
               && $var->sigil eq '$'
               && $var->name ne 'MATCH'  # XXX $MATCH is the Perl6 match object
               )
            {
                $var = Var->new( sigil => '%', twigil => $var->twigil, namespace => $var->namespace, name => $var->name );
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
                $var = Var->new( sigil => '@', twigil => $var->twigil, namespace => $var->namespace, name => $var->name );
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

class If {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $.cond;
        if (  $cond->isa( 'Var' )
           && $cond->sigil eq '@'
           )
        {
            $cond = Apply->new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $.body->stmts, needs_return => 0 );
        my $s = Javascript::tab($level) . 'if ( ' . ('bool') . '(' . $cond->emit_javascript() . ') ) { '
            . '(function () {' . "\n"
            .       $body->emit_javascript_indented( $level + 1 ) . "\n"
            . Javascript::tab($level) . '})(); }';
        if ( $.otherwise->stmts ) {
            my $otherwise = Perlito5::Javascript::LexicalBlock->new( block => $.otherwise->stmts, needs_return => 0 );
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


class While {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $.body->stmts, needs_return => 0 );
        return
           Javascript::tab($level) . 'for ( '
        .  ( $.init     ? $.init->emit_javascript()             . '; '  : '; ' )
        .  ( $.cond     ? ('bool') . '(' . $.cond->emit_javascript() . '); ' : '; ' )
        .  ( $.continue ? $.continue->emit_javascript()         . ' '   : ' '  )
        .  ') { '
            . '(function () { ' . $body->emit_javascript_indented( $level + 1 )      . ' })()'
        . ' }'
    }
}

class For {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $.cond;
        if (!( $cond->isa( 'Var' ) && $cond->sigil eq '@' )) {
            $cond = Lit::Array->new( array1 => [$cond] )
        }
        my $body      = Perlito5::Javascript::LexicalBlock->new( block => $.body->stmts, needs_return => 0 );
        my $sig = 'v__';
        if ($.body->sig()) {
            $sig = $.body->sig->emit_javascript_indented( $level + 1 );
        }
        Javascript::tab($level) . '(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { '
            . "(function ($sig) \{ "
                . $body->emit_javascript_indented( $level + 1 )
            . ' })(a_[i_]) } })'
        . '(' . $cond->emit_javascript() . ')'
    }
}

class Decl {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Javascript::tab($level) . $.var->emit_javascript;
    }
    sub emit_javascript_init {
        my $self = shift;
        if ($.decl eq 'my') {
            my $str = "";
            $str = $str . 'var ' . ($.var)->emit_javascript() . ' = ';
            if ($.var)->sigil eq '%' {
                $str = $str . '{};' . "\n";
            }
            elsif ($.var)->sigil eq '@' {
                $str = $str . '[];' . "\n";
            }
            else {
                $str = $str . 'null;' . "\n";
            }
            return $str;
        }
        else {
            die "not implemented: Decl '" . $.decl . "'";
        }
    }
}

class Sub {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        my $sig = $.sig;
        my $pos = $sig->positional;
        my $str = join( ', ', map( $_->emit_javascript(), @$pos ) );
          Javascript::tab($level) . 'function ' . $.name . '(' . $str . ') {' . "\n"
        . Javascript::tab($level + 1) . 'var List__ = Array.prototype.slice.call(arguments);' . "\n"
        . Javascript::tab($level + 1) . 'if (List__[0] instanceof CallSubClass) {' . "\n"
        . Javascript::tab($level + 2) .   'List__.shift()' . "\n"
        . Javascript::tab($level + 1) . '}' . "\n"
        . Javascript::tab($level + 1) . 'else {' . "\n"
        . Javascript::tab($level + 2) .   'List__.unshift(this)' . "\n"
        . Javascript::tab($level + 1) . '}' . "\n"
        .   (Perlito5::Javascript::LexicalBlock->new( block => $.block, needs_return => 1, top_level => 1 ))->emit_javascript_indented( $level + 1 ) . "\n"
        . Javascript::tab($level) . '}'
    }
}

class Do {
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

class Use {
    sub emit_javascript { $_[0]->emit_javascript_indented(0) }
    sub emit_javascript_indented {
        my $self = shift;
        my $level = shift;
        Javascript::tab($level) . '// use ' . $.mod . "\n"
    }
}

=begin

=head1 NAME

Perlito5::Javascript::Emit - Code generator for Perlito-in-Javascript

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

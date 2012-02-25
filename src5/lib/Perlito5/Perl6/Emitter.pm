use v5;

use Perlito5::AST;

package Perl6;
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
                push @out, "chr(" . ord($c) . ")";
                $tmp = '';
            }
        }
        push @out, "'$tmp'" if $tmp ne '';
        return join(' ~ ', @out);
    }

    sub to_str {
            my $cond = shift;
            if ($cond->isa( 'Val::Buf' )) {
                return $cond->emit_perl6;
            }
            else {
                return '(' . $cond->emit_perl6 . ')';  # XXX change if needed
            }
    }
    sub to_num {
            my $cond = shift;
            if ($cond->isa( 'Val::Int' ) || $cond->isa( 'Val::Num' )) {
                return $cond->emit_perl6;
            }
            else {
                return '(' . $cond->emit_perl6 . ')';   # XXX change if needed
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
                return $cond->emit_perl6;
            }
            else {
                return '(' . $cond->emit_perl6 . ')';  # XXX change if needed
            }
    }

}

package Perlito5::Perl6::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{'block'} }
    sub needs_return { $_[0]->{'needs_return'} }
    sub top_level { $_[0]->{'top_level'} }

    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;

        if ($self->{"top_level"}) {
            my $block = Perlito5::Perl6::LexicalBlock->new( block => $self->block, needs_return => $self->needs_return, top_level => 0 );
            return $block->emit_perl6_indented( $level + 1 ) . ';' . "\n"
        }

        my @block;
        for (@{$self->{"block"}}) {
            if (defined($_)) {
                push @block, $_
            }
        }
        if (!@block) {
            return Perl6::tab($level) . ';';
        }
        my @str;
        for my $decl ( @block ) {
            if ($decl->isa( 'Decl' ) && $decl->decl eq 'my') {
                push @str, Perl6::tab($level) . $decl->emit_perl6_init;
            }
            if ($decl->isa( 'Apply' ) && $decl->code eq 'infix:<=>') {
                my $var = $decl->arguments[0];
                if ($var->isa( 'Decl' ) && $var->decl eq 'my') {
                    push @str, Perl6::tab($level) . $var->emit_perl6_init;
                }
            }
        }
        for my $decl ( @block ) {
            if (!( $decl->isa( 'Decl' ) && $decl->decl eq 'my' )) {
                push @str, $decl->emit_perl6_indented($level) . ';';
            }
        }
        return join("\n", @str) . ';';
    }
}

package CompUnit;
{
    sub emit_perl6 { 
        my $self = $_[0];
        $self->emit_perl6_indented(0) 
    }
    sub emit_perl6_indented {
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

        my $class_name = $self->{"name"};
        my $str = 'package ' . $class_name . '{' . "\n";

        for my $decl ( @body ) {
            if ($decl->isa( 'Decl' ) && ( $decl->decl eq 'my' )) {
                $str = $str . '  ' . $decl->emit_perl6_init;
            }
            if ($decl->isa( 'Apply' ) && $decl->code eq 'infix:<=>') {
                my $var = $decl->arguments[0];
                if ($var->isa( 'Decl' ) && $var->decl eq 'my') {
                    $str = $str . '  ' . $var->emit_perl6_init;
                }
            }
        }
        for my $decl ( @body ) {
            if ($decl->isa( 'Sub' )) {
                $str = $str . ($decl)->emit_perl6_indented( $level + 1 ) . ";\n";
            }
        }
        for my $decl ( @body ) {
            if (  defined( $decl )
               && (!( $decl->isa( 'Decl' ) && $decl->decl eq 'my' ))
               && (!( $decl->isa( 'Sub')))
               )
            {
                $str = $str . ($decl)->emit_perl6_indented( $level + 1 ) . ";\n";
            }
        }
        $str . "}\n";
    }
    sub emit_perl6_program {
        my $comp_units = shift;
        my $str = '';
        $str .= "use Perlito5::Perl6::Runtime;\n";
        for my $comp_unit ( @$comp_units ) {
            $str = $str . $comp_unit->emit_perl6() . "\n";
        }
        return $str;
    }
}

package Val::Int;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift; Perl6::tab($level) . $self->{"int"} }
}

package Val::Num;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift; Perl6::tab($level) . $self->{"num"} }
}

package Val::Buf;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift; Perl6::tab($level) . Perl6::escape_string($self->{"buf"}) }
}

package Lit::Block;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $sig = 'v__';
        if ($self->{"sig"}) {
            $sig = $self->{"sig"}->emit_perl6_indented( $level + 1 );
        }
        return
              Perl6::tab($level) . "(function ($sig) \{\n"
            .   (Perlito5::Perl6::LexicalBlock->new( block => $self->{"stmts"}, needs_return => 1 ))->emit_perl6_indented( $level + 1 ) . "\n"
            . Perl6::tab($level) . '})'
    }
}

package Lit::Array;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $ast = $self->expand_interpolation;
        return $ast->emit_perl6_indented( $level );
    }
}

package Lit::Hash;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $ast = $self->expand_interpolation;
        return $ast->emit_perl6_indented( $level );
    }
}

package Index;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;

        if (  $self->{"obj"}->isa('Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Var->new( sigil => '@', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_perl6_indented($level) . '[' . $self->{"index_exp"}->emit_perl6() . ']';
        }

        Perl6::tab($level) . $self->{"obj"}->emit_perl6() . '[' . $self->{"index_exp"}->emit_perl6() . ']';
    }
}

package Lookup;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;

        if (  $self->{"obj"}->isa('Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Var->new( sigil => '%', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_perl6_indented($level) . '{' . $self->{"index_exp"}->emit_perl6() . '}';
        }
        return $self->{"obj"}->emit_perl6_indented($level) . '{' . $self->{"index_exp"}->emit_perl6() . '}';
    }
}

package Var;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;

        if ( $self->{"sigil"} eq '*' ) {
            my $ns = 'v__NAMESPACE';
            if ($self->{"namespace"}) {
                $ns = 'NAMESPACE["' . $self->{"namespace"} . '"]';
            }
            return $ns . '::' . $self->{"name"};
        }

        my $ns = '';
        if ($self->{"namespace"}) {
            $ns = $self->{"namespace"} . '::';
        }
        $ns . $self->{"sigil"} . $self->{"name"}
    }
    sub plain_name {
        my $self = shift;
        if ($self->namespace) {
            return $self->namespace . '::' . $self->name
        }
        return $self->name
    }
}

package Proto;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        Perl6::tab($level) . $self->{"name"}
    }
}

package Call;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $invocant = $self->{"invocant"}->emit_perl6;
        my $meth = $self->{"method"};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            return Perl6::tab($level) . $invocant . '[' . $self->{"arguments"}->emit_perl6() . ']'
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            return Perl6::tab($level) . $invocant . '{' . $self->{"arguments"}->emit_perl6() . '}'
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            my @args = ();
            push @args, $_->emit_perl6
                for @{$self->{"arguments"}};
            return Perl6::tab($level) . '(' . $invocant . ')(' . join(',', @args) . ')';
        }
        # try to call a method on the class; if that fails, then call a 'native js' method
        my @args = ($invocant);
        push @args, $_->emit_perl6
            for @{$self->{"arguments"}};
        return Perl6::tab($level) . $invocant . '.' . $meth . '(' . join(',', @args) . ')'
    }
}

package Apply;
{

    my %op_infix_js = (
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:<x>'  => ' x ',
        'infix:<+>'  => ' + ',
        'infix:<.>'  => ' ~ ',
        'infix:</>'  => ' / ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',

        'infix:<eq>' => ' eq ',
        'infix:<ne>' => ' ne ',
        'infix:<le>' => ' le ',
        'infix:<ge>' => ' ge ',

        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',
        'infix:<..>' => ' .. ',
        'infix:<&&>' => ' && ',
        'infix:<||>' => ' || ',
        'infix:<and>' => ' and ',
        'infix:<or>' => ' or ',
        'infix:<//>' => ' // ',
    );

    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_perl6_indented( $level );
        }

        my $code = $self->{"code"};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_perl6
                for @{$self->{"arguments"}};
            return Perl6::tab($level) . '(' . $self->{"code"}->emit_perl6() . ')(' . join(',', @args) . ')';
        }
        if ($code eq 'infix:<=>>') {
            return Perl6::tab($level) . join(', ', map( $_->emit_perl6, @{$self->{"arguments"}} ))
        }
        if (exists $op_infix_js{$code}) {
            return Perl6::tab($level) . '(' 
                . join( $op_infix_js{$code}, map( $_->emit_perl6, @{$self->{"arguments"}} ))
                . ')'
        }

        if ($code eq 'eval') {
            return
                'eval(perl5_to_js(' 
                    . Perl6::to_str($self->{"arguments"}->[0])
                . '))'
        }

        if ($code eq 'undef')      { return Perl6::tab($level) . 'Any' }

        if ($code eq 'shift')      {
            if (!( $self->{"arguments"} && @{$self->{"arguments"}} )) {
                return 'shift(@_)'
            }
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
                            . 'out.push(' . $fun->emit_perl6 . ')'
                        . '}; '
                        . 'return out;'
                    . ' })(' . $list->emit_perl6() . ')'
        }

        if (  $code eq 'bless' 
           || $code eq 'ref'
           )
        {
            return 'Perlito5::Perl6::Runtime::' . $code . '( ' 
                . join( ', ', map( $_->emit_perl6, @{ $self->{"arguments"} } ) )
                . ')';
        }
        if ( $code eq 'prefix:<!>' ) {
            return '!( ' . Perl6::to_bool( $self->{"arguments"}->[0] ) . ')';
        }

        if ( $code eq 'prefix:<$>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '$(' . $arg->emit_perl6 . ')';
        }
        if ( $code eq 'prefix:<@>' ) {
            return '@(' . join( ' ', map( $_->emit_perl6, @{ $self->{"arguments"} } ) ) . ')';
        }
        if ( $code eq 'prefix:<%>' ) {
            my $arg = $self->{"arguments"}->[0];
            return '%(' . $arg->emit_perl6 . ')';
        }

        if ( $code eq 'circumfix:<[ ]>' ) {
            return '[' . join( ', ', map( $_->emit_perl6, @{ $self->{"arguments"} } ) ) . ']';
        }
        if ( $code eq 'prefix:<\\>' ) {
            my $arg = $self->{"arguments"}->[0];
            if ( $arg->isa('Var') ) {
                if ( $arg->sigil eq '@' ) {
                    # XXX not implemented
                    return $arg->emit_perl6;
                }
                if ( $arg->sigil eq '%' ) {
                    return '(HashRef.new(' . $arg->emit_perl6 . '))';
                }
            }
            # XXX \&x should return a CODE ref
            return '(ScalarRef.new(' . $arg->emit_perl6 . '))';
        }

        if ($code eq 'postfix:<++>') { return '('   . join(' ', map( $_->emit_perl6, @{$self->{"arguments"}} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return '('   . join(' ', map( $_->emit_perl6, @{$self->{"arguments"}} ))  . ')--' }
        if ($code eq 'prefix:<++>')  { return '++(' . join(' ', map( $_->emit_perl6, @{$self->{"arguments"}} ))  . ')' }
        if ($code eq 'prefix:<-->')  { return '--(' . join(' ', map( $_->emit_perl6, @{$self->{"arguments"}} ))  . ')' }

        if ($code eq 'prefix:<+>') { return '+('  . $self->{"arguments"}->[0]->emit_perl6()  . ')' }

        if ($code eq 'list:<.>')
        {
            return '('
                . join( ' ~ ',
                        map( Perl6::to_str($_), @{$self->{"arguments"}} )
                      )
                . ')'
        }

        if ($code eq 'ternary:<?? !!>') {
            return Perl6::tab($level) 
                 . '( ' . Perl6::to_bool( $self->{"arguments"}->[0] )
                 . ' ?? ' . ($self->{"arguments"}->[1])->emit_perl6()
                 . ' !! ' . ($self->{"arguments"}->[2])->emit_perl6()
                 . ')'
        }
        if ($code eq 'circumfix:<( )>') {
            return Perl6::tab($level) . '(' . join(', ', map( $_->emit_perl6, @{$self->{"arguments"}} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return emit_perl6_bind( $self->{"arguments"}->[0], $self->{"arguments"}->[1], $level );
        }
        if ($code eq 'return') {
            return Perl6::tab($level) . 'return('
                .   ( $self->{"arguments"} && @{$self->{"arguments"}} 
                    ? $self->{"arguments"}->[0]->emit_perl6() 
                    : ''
                    )
                . ')'
        }

        if ($self->{"namespace"}) {

            if (  $self->{"namespace"} eq 'Perl6' 
               && $code eq 'inline'
               ) 
            {
                if ( $self->{"arguments"}->[0]->isa('Val::Buf') ) {
                    # Perl6::inline('$x = 123')
                    return $self->{"arguments"}[0]{"buf"};
                }
                else {
                    die "Perl6::inline needs a string constant";
                }
            }

            $code = $self->{"namespace"} . '::' . ( $code );
        }
        my @args = ();
        push @args, $_->emit_perl6
            for @{$self->{"arguments"}};
        Perl6::tab($level) . $code . '(' . join(', ', @args) . ')';
    }

    sub emit_perl6_bind {
        my $parameters = shift;
        my $arguments = shift;
        my $level = shift;
        Perl6::tab($level) . '(' . $parameters->emit_perl6() . ' = ' . $arguments->emit_perl6() . ')';
    }
}

package If;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{"cond"};
        if (  $cond->isa( 'Var' )
           && $cond->sigil eq '@'
           )
        {
            $cond = Apply->new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
        my $body  = Perlito5::Perl6::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        my $s = Perl6::tab($level) . 'if ( ' . Perl6::to_bool( $cond ) . ' ) {' . "\n"
            .       $body->emit_perl6_indented( $level + 1 ) . "\n"
            . Perl6::tab($level) . '}';
        if ( @{ $self->{"otherwise"}->stmts } ) {
            my $otherwise = Perlito5::Perl6::LexicalBlock->new( block => $self->{"otherwise"}->stmts, needs_return => 0 );
            $s = $s
                . "\n"
                . Perl6::tab($level) . 'else {' . "\n"
                .       $otherwise->emit_perl6_indented( $level + 1 ) . "\n"
                . Perl6::tab($level) . '}';
        }
        return $s;
    }
}


package While;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $body      = Perlito5::Perl6::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        return
           Perl6::tab($level) . 'loop ( '
        .  ( $self->{"init"}     ? $self->{"init"}->emit_perl6()           . '; '  : '; ' )
        .  ( $self->{"cond"}     ? Perl6::to_bool( $self->{"cond"} )       . '; '  : '; ' )
        .  ( $self->{"continue"} ? $self->{"continue"}->emit_perl6()       . ' '   : ' '  )
        .  ') {' . "\n" 
            . $body->emit_perl6_indented( $level + 1 )
        . ' }'
    }
}

package For;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $cond = $self->{"cond"};
        if (!( $cond->isa( 'Var' ) && $cond->sigil eq '@' )) {
            $cond = Lit::Array->new( array1 => [$cond] )
        }
        my $body      = Perlito5::Perl6::LexicalBlock->new( block => $self->{"body"}->stmts, needs_return => 0 );
        my $sig = 'v__';
        if ($self->{"body"}->sig()) {
            $sig = $self->{"body"}->sig->emit_perl6_indented( $level + 1 );
        }
        Perl6::tab($level) . '(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { '
            . "(function ($sig) {\n"
                . $body->emit_perl6_indented( $level + 1 )
            . ' })(a_[i_]) } })'
        . '(' . $cond->emit_perl6() . ')'
    }
}

package Decl;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        Perl6::tab($level) . $self->{"var"}->emit_perl6;
    }
    sub emit_perl6_init {
        my $self = shift;
        $self->{"decl"} . ' ' . ($self->{"var"})->emit_perl6() . ';';
    }
}

package Sub;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;

          Perl6::tab($level)
        . "sub "
        . ( $self->{"name"}
          ? $self->{"name"}
          : ''
          )
        . '(*@_) {' . "\n"
        .   (Perlito5::Perl6::LexicalBlock->new( block => $self->{"block"}, needs_return => 1, top_level => 1 ))->emit_perl6_indented( $level + 1 ) . "\n"
        . Perl6::tab($level) . '}';

    }
}

package Do;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $block = $self->simplify->block;
        return
              Perl6::tab($level) . '(do {' . "\n"
            .   (Perlito5::Perl6::LexicalBlock->new( block => $block, needs_return => 1 ))->emit_perl6_indented( $level + 1 ) . "\n"
            . Perl6::tab($level) . '})'
    }
}

package Use;
{
    sub emit_perl6 { $_[0]->emit_perl6_indented(0) }
    sub emit_perl6_indented {
        my $self = shift;
        my $level = shift;
        my $mod = $self->{"mod"};
        return 
            if $mod eq 'feature' 
            || $mod eq 'strict'
            || $mod eq 'v5';
        Perl6::tab($level) . 'use ' . $self->{"mod"} . ";"
    }
}

=begin

=head1 NAME

Perlito5::Perl6::Emit - Code generator for Perlito Perl5-in-Perl6

=head1 SYNOPSIS

    $program->emit_perl6()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Perl6 code for the Perlito Perl 5 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

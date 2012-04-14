use v5;

use Perlito5::AST;

package Perlito5::Perl5;
{
    sub tab {
        my $level = shift;
        "    " x $level
    }

    my %safe_char = (
        ' ' => 1,
        '!' => 1,
        '"' => 1,
        '#' => 1,
        '$' => 1,
        '%' => 1,
        '&' => 1,
        '(' => 1,
        ')' => 1,
        '*' => 1,
        '+' => 1,
        ',' => 1,
        '-' => 1,
        '.' => 1,
        '/' => 1,
        ':' => 1,
        ';' => 1,
        '<' => 1,
        '=' => 1,
        '>' => 1,
        '?' => 1,
        '@' => 1,
        '[' => 1,
        ']' => 1,
        '^' => 1,
        '_' => 1,
        '`' => 1,
        '{' => 1,
        '|' => 1,
        '}' => 1,
        '~' => 1,
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
        return join(' . ', @out);
    }

}

package Perlito5::AST::CompUnit;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];

        my @body;
        for (@{$self->{body}}) {
            if (defined($_)) {
                push @body, $_
            }
        }
          Perlito5::Perl5::tab($level)    . 'package ' . $self->{name} . ";" . "\n"
        .                         join(";\n", map( $_->emit_perl5( $level ), @body )) . ";\n"
        . "\n"
    }
    sub emit_perl5_program {
        my $comp_units = $_[0];

        my $str = ''
            . "use v5;\n"
            . "use Perlito5::Perl5::Runtime;\n";
        for my $comp_unit (@{$comp_units}) {
            $str .= $comp_unit->emit_perl5(0)
        }
        $str .= "1;\n";
        return $str;
    }
}

package Perlito5::AST::Val::Int;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
         Perlito5::Perl5::tab($level) . $self->{int} }
}

package Perlito5::AST::Val::Num;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
         Perlito5::Perl5::tab($level) . $self->{num} }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        Perlito5::Perl5::tab($level) . Perlito5::Perl5::escape_string($self->{buf})
    }
}

package Perlito5::AST::Lit::Block;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
          Perlito5::Perl5::tab($level) . "for (\$_) {\n"
        .   join(";\n", map( $_->emit_perl5( $level + 1 ), @{$self->{stmts}} )) . "\n"
        . Perlito5::Perl5::tab($level) . "}"
    }
}

package Perlito5::AST::Index;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = $self->{obj};
            return $v->emit_perl5($level) . '[' . $self->{index_exp}->emit_perl5() . ']';
        }

        $self->{obj}->emit_perl5($level) . '->[' . $self->{index_exp}->emit_perl5() . ']';
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = $self->{obj};
            return $v->emit_perl5($level) . '{' . $self->autoquote($self->{index_exp})->emit_perl5($level) . '}';
        }

        $self->{obj}->emit_perl5($level) . '->{' . $self->autoquote($self->{index_exp})->emit_perl5($level) . '}';
    }
}

package Perlito5::AST::Var;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        # Normalize the sigil
        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        return Perlito5::Perl5::tab($level) . $self->{sigil} . $ns . $self->{name}
    }
}

package Perlito5::AST::Proto;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        Perlito5::Perl5::tab($level) . $self->{name}
    }
}

package Perlito5::AST::Call;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $invocant = $self->{invocant}->emit_perl5;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::Perl5::tab($level) . $invocant . '->[' . $self->{arguments}->emit_perl5() . ']'
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return Perlito5::Perl5::tab($level) . $invocant . '->{' . Perlito5::AST::Lookup->autoquote($self->{arguments})->emit_perl5($level) . '}'
        }
        my $meth = $self->{method};
        if  ($meth eq 'postcircumfix:<( )>')  {
             $meth = '';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_perl5();
        }
        Perlito5::Perl5::tab($level) . $invocant . '->' . $meth . '(' . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . ')';
    }
}

package Perlito5::AST::Apply;
{

    my %op_prefix_perl5 = (
        say     => 'Perlito5::Runtime::say',
        print   => 'print',
        keys    => 'keys',
        values  => 'values',
        warn    => 'warn',
        'prefix:<!>'    => '!',
        'prefix:<++>'   => '++',
        'prefix:<-->'   => '--',
        'prefix:<+>'    => '+',
        'prefix:<->'    => '-',
        'prefix:<-f>'   => '-f',
    );

    my %op_infix_perl5 = (
        'list:<,>'   => ', ',
        'list:<.>'   => ' . ',
        'infix:<+>'  => ' + ',
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<**>' => ' ** ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',

        'infix:<&>'  => ' & ',
        'infix:<|>'  => ' | ',
        'infix:<^>'  => ' ^ ',
        'infix:<&&>' => ' && ',
        'infix:<||>' => ' || ',
        'infix:<and>' => ' and ',
        'infix:<or>' => ' or ',
        'infix:<//>' => ' // ',
        'infix:<eq>' => ' eq ',
        'infix:<ne>' => ' ne ',
        'infix:<le>' => ' le ',
        'infix:<ge>' => ' ge ',

        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',

        'infix:<=~>' => ' =~ ',
        'infix:<!~>' => ' !~ ',
    );

    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_perl5( $level );
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        my $code = $ns . $self->{code};

        if (ref $code ne '') {
            return Perlito5::Perl5::tab($level) . '(' . $self->{code}->emit_perl5() . ')->(' . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . ')';
        }

        if (exists $op_infix_perl5{$code}) {
            return Perlito5::Perl5::tab($level) . '(' 
                . join( $op_infix_perl5{$code},
                        (map( $_->emit_perl5, @{$self->{arguments}} ))
                      )
                . ')'
        }
        if (exists $op_prefix_perl5{$code}) {
            return Perlito5::Perl5::tab($level) . $op_prefix_perl5{$code} . '('   . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . ')'
        }

        if ($self->{code} eq 'p5:s') {
            return Perlito5::Perl5::tab($level)
                . 's!' . $self->{arguments}->[0]->{buf}   # emit_perl5() 
                .  '!' . $self->{arguments}->[1]->{buf}   # emit_perl5()
                .  '!' . $self->{arguments}->[2];

        }
        if ($self->{code} eq 'p5:m') {

            my $s;
            if ($self->{arguments}->[0]->isa('Perlito5::AST::Val::Buf')) {
                $s = $self->{arguments}->[0]->{buf}
            }
            else {
                for my $ast (@{$self->{arguments}[0]{arguments}}) {
                    if ($ast->isa('Perlito5::AST::Val::Buf')) {
                        $s .= $ast->{buf}
                    }
                    else {
                        $s .= $ast->emit_perl5();  # variable name
                    }
                }
            }

            return Perlito5::Perl5::tab($level)
                . 'm!' . $s . '!' . $self->{arguments}->[1];
        }

        if ($code eq '__PACKAGE__') {
            return '"' . $Perlito5::PKG_NAME . '"';
        }
        if ($self->{code} eq 'package')    { return Perlito5::Perl5::tab($level) . 'package ' . $self->{namespace} }
        if ($code eq 'undef')      { return Perlito5::Perl5::tab($level) . 'undef()' }

        if ($code eq 'scalar')     { return Perlito5::Perl5::tab($level) . 'scalar(' . ($self->{arguments}->[0]->emit_perl5) . ')' }
        if ($code eq 'pop')        { return Perlito5::Perl5::tab($level) . 'pop('  . ($self->{arguments}->[0]->emit_perl5) . ')' }
        if ($code eq 'push')       { return Perlito5::Perl5::tab($level) . 'push(' . ($self->{arguments}->[0]->emit_perl5) . ', ' . ($self->{arguments}->[1])->emit_perl5() . ' )' }
        if ($code eq 'shift')      { 
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return Perlito5::Perl5::tab($level) . 'shift(' . join(' ', map( $_->emit_perl5, @{$self->{arguments}} ))    . ')' 
            }
            return 'shift()'
        }
        if ($code eq 'unshift')    { return Perlito5::Perl5::tab($level) . 'unshift(' . $self->{arguments}->[0]->emit_perl5()  . ', ' . $self->{arguments}->[1]->emit_perl5() . ')' }

        if ($code eq 'map')       {    
            my $str = shift @{$self->{arguments}};
            return Perlito5::Perl5::tab($level) . 'map(' . $str->emit_perl5 . ', ' . join(',', map( $_->emit_perl5, @{$self->{arguments}} )) . ')'
        }

        if ($code eq 'infix:<x>')  { 
            return 'join("", ' . join(' x ', map( $_->emit_perl5, @{$self->{arguments}} ))  . ')'
        }
        if ($code eq 'infix:<=>>')  { 
            return Perlito5::Perl5::tab($level) 
                . Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_perl5($level)  . ', ' 
                . $self->{arguments}[1]->emit_perl5($level)
        }

        if ($code eq 'join')       {    
            my $str = shift @{$self->{arguments}};
            return Perlito5::Perl5::tab($level) . 'join(' . $str->emit_perl5 . ', ' . join(',', map( $_->emit_perl5, @{$self->{arguments}} )) . ')'
        }

        if ($code eq 'circumfix:<[ ]>') { 
            return Perlito5::Perl5::tab($level) . '[' . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . ']'
        }
        if ($code eq 'circumfix:<{ }>') { 
            return Perlito5::Perl5::tab($level) . '{' . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . '}'
        }
        if ($code eq 'prefix:<\\>') { 
            # TODO - \(@a) vs. \@a
            return Perlito5::Perl5::tab($level) . '\\' . join(' ', map( $_->emit_perl5, @{$self->{arguments}} )) . ''
        }
        if ($code eq 'prefix:<$>') { return Perlito5::Perl5::tab($level) . '${' . join(' ', map( $_->emit_perl5, @{$self->{arguments}} ))     . '}' }
        if ($code eq 'prefix:<@>') { return Perlito5::Perl5::tab($level) . '@{' . join(' ', map( $_->emit_perl5, @{$self->{arguments}} ))     . '}' }
        if ($code eq 'prefix:<%>') { return Perlito5::Perl5::tab($level) . '%{' . join(' ', map( $_->emit_perl5, @{$self->{arguments}} ))     . '}' }
        if ($code eq 'prefix:<&>') { return Perlito5::Perl5::tab($level) . '&{' . join(' ', map( $_->emit_perl5, @{$self->{arguments}} ))     . '}' }


        if ($code eq 'postfix:<++>') { return Perlito5::Perl5::tab($level) . '('   . join(' ', map( $_->emit_perl5, @{$self->{arguments}} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return Perlito5::Perl5::tab($level) . '('   . join(' ', map( $_->emit_perl5, @{$self->{arguments}} ))  . ')--' }

        if ($code eq 'infix:<..>') { return Perlito5::Perl5::tab($level) . '('  . join(' .. ', map( $_->emit_perl5, @{$self->{arguments}} ))  . ")" }

        if ($code eq 'ternary:<? :>') {
            return Perlito5::Perl5::tab($level)
                .  '('  . $self->{arguments}->[0]->emit_perl5
                . ' ? ' . $self->{arguments}->[1]->emit_perl5
                . ' : ' . $self->{arguments}->[2]->emit_perl5
                .  ')'
        }

        if ($code eq 'circumfix:<( )>') {
            return Perlito5::Perl5::tab($level) . '(' . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return Perlito5::Perl5::tab($level) . emit_perl5_bind( $self->{arguments}->[0], $self->{arguments}->[1] );
        }

        if ($code eq 'require') {
            return 'Perlito5::Grammar::Use::require(' . $self->{arguments}[0]->emit_perl5() . ')'
        }

        if ($code eq 'do') {
            # Note: this is "do EXPR" - look at the "Do" AST node for "do BLOCK"
            my $ast =
                Perlito5::AST::Apply->new(
                    code => 'eval',
                    namespace => '',
                    arguments => [
                       Perlito5::AST::Apply->new(
                          code => 'slurp',
                          namespace => 'Perlito5::IO',
                          arguments => $self->{arguments}
                        )
                    ]
                );
            return $ast->emit_perl5( $level );
        }

        if ($code eq 'eval') {
            my $arg = $self->{arguments}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Do" )) {
                $eval = $arg->emit_perl5( $level + 1, $wantarray );
            }
            else {
                $eval = 
                    '(do { '
                    .   'my $m = Perlito5::Grammar->exp_stmts(' 
                    .       '"do {" . ' . $arg->emit_perl5( $level + 1, 'scalar' ) . ' . "}", 0);'
                    .   'my $source = Perlito5::Match::flat($m)->[0]->emit_perl5(0, "scalar");'
                    # .   'warn $source;'
                    .   'eval $source;'
                    . '})';
            }
            return Perlito5::Perl5::tab($level) . $eval;
        }

        if ($code eq 'return') {
            return Perlito5::Perl5::tab($level) . 'return (' . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . ')';
        }

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            return Perlito5::Perl5::tab($level) . $code;
        }

        Perlito5::Perl5::tab($level) . $code . '(' . join(', ', map( $_->emit_perl5, @{$self->{arguments}} )) . ')';
    }

    sub emit_perl5_bind {
        my $parameters = shift;
        my $arguments = shift;

        if ($parameters->isa( 'Perlito5::AST::Call' )) {

            # $a->{3} = 4
            # $a->[3] = 4
            if  (  $parameters->method eq 'postcircumfix:<{ }>'
                || $parameters->method eq 'postcircumfix:<[ ]>'
                )
            {
                return '(' . $parameters->emit_perl5() . ' = ' . $arguments->emit_perl5() . ')';
            }

        }
        '(' . $parameters->emit_perl5() . ' = ' . $arguments->emit_perl5() . ')';
    }
}

package Perlito5::AST::If;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        return Perlito5::Perl5::tab($level) . 'if (' . $self->{cond}->emit_perl5() . ") \{\n"
             .  ($self->{body}
                ? join(";\n", 
                       map( $_->emit_perl5( $level + 1 ), @{ $self->{body}->stmts } )
                  ) . "\n"
                : ''
                )
             . Perlito5::Perl5::tab($level) . "}"
             .  ($self->{otherwise} && scalar(@{ $self->{otherwise}->stmts })
                ?  ( "\n"
                    . Perlito5::Perl5::tab($level) . "else \{\n"
                    .   join( ";\n", 
                              map( $_->emit_perl5( $level + 1 ), @{ $self->{otherwise}->stmts } )
                        ) . "\n"
                    . Perlito5::Perl5::tab($level) . "}"
                    )
                : ''
                );
    }
}

package Perlito5::AST::While;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $self->{cond};
           Perlito5::Perl5::tab($level) . 'for ( '
        .  ( $self->{init}     ? $self->{init}->emit_perl5()           . '; ' : '; ' )
        .  ( $cond      ? $cond->emit_perl5()            . '; ' : '; ' )
        .  ( $self->{continue} ? $self->{continue}->emit_perl5()       . ' '  : ' '  )
        .  ') {' . "\n"
        .       join(";\n", 
                     map( $_->emit_perl5( $level + 1 ), @{ $self->{body}->stmts } )
                    ) . "\n"
        . Perlito5::Perl5::tab($level) . "}"
    }
}

package Perlito5::AST::For;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $self->{cond};
        my $sig;
        if ($self->{body}->sig()) {
            $sig = 'my ' . $self->{body}->sig->emit_perl5() . ' ';
        }
        return  Perlito5::Perl5::tab($level) . 'for ' . $sig . '(' . $cond->emit_perl5() . ') {' . "\n"
             .   join(";\n", map( $_->emit_perl5( $level + 1 ), @{ $self->{body}->stmts } )) . "\n"
             . Perlito5::Perl5::tab($level) . "}"
    }
}

package Perlito5::AST::Decl;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $decl = $self->{decl};
        my $str =
              '(' . $self->{decl} . ' ' . $self->{type} . ' ' . $self->{var}->emit_perl5() . ')';
        return Perlito5::Perl5::tab($level) . $str;
    }
}

package Perlito5::AST::Sub;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $name = '';
        $name = $self->{namespace} . "::" . $self->{name} . " "
            if $self->{name};

        my $sig = $self->{sig};
        my $i = 0;
          Perlito5::Perl5::tab($level) . 'sub ' . $name . "{\n"
        .   join(";\n", map( $_->emit_perl5( $level + 1 ), @{$self->{block}} )) . "\n"
        . Perlito5::Perl5::tab($level) . "}"
    }
}

package Perlito5::AST::Do;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $block = $self->simplify->block;
          Perlito5::Perl5::tab($level) . "(do \{\n"
        .   join(";\n", map( defined($_) && $_->emit_perl5( $level + 1 ), @$block )) . "\n"
        . Perlito5::Perl5::tab($level) . "})"
    }
}

package Perlito5::AST::Use;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        if  (  $self->{mod} eq 'strict'
            || $self->{mod} eq 'feature'
            )
        {
            return "\n"
                . Perlito5::Perl5::tab($level) . "# " . $self->{code} . " " . $self->{mod} . "\n"
        }
        Perlito5::Perl5::tab($level) . $self->{code} . ' ' . $self->{mod}
    }
}

=begin

=head1 NAME

Perlito5::Perl5::Emit - Code generator for Perlito5-in-Perl5

=head1 SYNOPSIS

    $program->emit_perl5()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Perl5 code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

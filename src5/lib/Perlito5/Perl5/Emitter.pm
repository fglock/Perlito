use v5;

use Perlito5::AST;

package Perlito5::Perl5;
{
    sub tab {
        my $level = shift;
        "    " x $level
    }

    sub escape_string {
        return Perlito5::Dumper::escape_string($_[0]);
    }

    sub emit_perl5_block {
        my $block = $_[0];
        my $level = $_[1];
          "{\n"
        .   join(";\n", 
                map( defined($_) && Perlito5::Perl5::tab($level+1) . $_->emit_perl5( $level + 1 ), @$block ))
        . "\n" . Perlito5::Perl5::tab($level) . "}"
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
           'package ' . $self->{name} . ";" . "\n"
        .        join(";\n", map( Perlito5::Perl5::tab($level) . $_->emit_perl5( $level ), @body )) . ";\n"
        . "\n"
    }
    sub emit_perl5_program {
        my $comp_units = $_[0];

        my $str = "use v5.10;\n";    # because we might use 'when' and 'say'
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
        my $self  = $_[0];
        my $level = $_[1];
        $self->{int};
    }
}

package Perlito5::AST::Val::Num;
{
    sub emit_perl5 {
        my $self  = $_[0];
        my $level = $_[1];
        $self->{num};
    }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_perl5 {
        my $self  = $_[0];
        my $level = $_[1];
        Perlito5::Perl5::escape_string( $self->{buf} );
    }
}

package Perlito5::AST::Lit::Block;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        # TODO - emit "continue" block
        Perlito5::Perl5::emit_perl5_block($self->{stmts}, $level);
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
            return $v->emit_perl5($level) . '[' . $self->{index_exp}->emit_perl5($level+1) . ']';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return $self->{obj}{arguments}[0]->emit_perl5($level) . '->[' . $self->{index_exp}->emit_perl5($level) . ']';
        }
        $self->{obj}->emit_perl5($level) . '->[' . $self->{index_exp}->emit_perl5($level+1) . ']';
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

        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return $self->{obj}{arguments}[0]->emit_perl5($level) . '->{' . $self->autoquote($self->{index_exp})->emit_perl5($level) . '}';
        }

        $self->{obj}->emit_perl5($level) . '->{' . $self->autoquote($self->{index_exp})->emit_perl5($level) . '}';
    }
}

package Perlito5::AST::Var;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];

        my $str_name = $self->{name};
        $str_name = '\\\\' if $str_name eq '\\';   # escape $\
        $str_name = '\\"' if $str_name eq '"';     # escape $"

        my $perl5_name = $self->perl5_name;
        # say "looking up $perl5_name";
        my $decl_type;  # my, our, local
        my $decl = $self->perl5_get_decl( $perl5_name );
        if ( $decl ) {
            # say "found ", $decl->{decl};
            $decl_type = $decl->{decl};
        }
        else {
            if ( !$self->{namespace}
               && $self->{sigil} ne '*'
               )
            {
                # TODO - track globals; see javascript emitter
                # if ( $Perlito5::STRICT ) {
                #    die "Global symbol \"$perl5_name\" requires explicit package name"
                # }
            }
        }

        # Normalize the sigil
        my $ns = '';
        if ($self->{namespace}) {
            if ($self->{namespace} eq 'main' && substr($self->{name}, 0, 1) eq '^') {
                # don't add the namespace to special variables
                return $self->{sigil} . '{' . $self->{name} . '}'
            }
            else {
                $ns = $self->{namespace} . '::';
            }
        }
        my $c = substr($self->{name}, 0, 1);
        if (  ($c ge 'a' && $c le 'z')
           || ($c ge 'A' && $c le 'Z')
           || ($c eq '_')
           ) 
        {
            return $self->{sigil} . $ns . $self->{name}
        }
        return $self->{sigil} . "{'" . $ns . $str_name . "'}"
    }
}

package Perlito5::AST::Proto;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        $self->{name}
    }
}

package Perlito5::AST::Call;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $invocant = $self->{invocant}->emit_perl5($level+1);
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return $invocant . '->[' . $self->{arguments}->emit_perl5($level+1) . ']'
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return $invocant . '->{' . Perlito5::AST::Lookup->autoquote($self->{arguments})->emit_perl5($level) . '}'
        }
        my $meth = $self->{method};
        if  ($meth eq 'postcircumfix:<( )>')  {
            if (  ref($self->{invocant}) eq 'Perlito5::AST::Var'
               && $self->{invocant}{sigil} eq '&'
               )
            {
                #  &subr(args)
                return $invocant . '(' . join(', ', map( $_->emit_perl5($level+1), @{$self->{arguments}} )) . ')';
            }
            $meth = '';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_perl5($level+1);
        }
        $invocant . '->' . $meth . '(' . join(', ', map( $_->emit_perl5($level+1), @{$self->{arguments}} )) . ')';
    }
}

package Perlito5::AST::Apply;
{

    my %op_prefix_perl5 = (
        say      => 'say',
        print    => 'print',
        keys     => 'keys',
        values   => 'values',
        warn     => 'warn',
        scalar   => 'scalar',
        pop      => 'pop',
        push     => 'push',
        shift    => 'shift',
        unshift  => 'unshift',
        join     => 'join',
        undef    => 'undef',
        'prefix:<!>'    => '!',
        'prefix:<++>'   => '++',
        'prefix:<-->'   => '--',
        'prefix:<+>'    => '+',
        'prefix:<->'    => '-',
        'prefix:<-d>'   => '-d',
        'prefix:<-e>'   => '-e',
        'prefix:<-f>'   => '-f',
        'prefix:<not>'  => 'not',
        'prefix:<~>'    => '~',
    );

    my %op_infix_perl5 = (
        'list:<,>'   => ', ',
        'list:<.>'   => ' . ',
        'infix:<+>'  => ' + ',
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<%>'  => ' % ',
        'infix:<**>' => ' ** ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',
        'infix:<<<>' => ' << ',
        'infix:<>>>' => ' >> ',

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
        'infix:<lt>' => ' lt ',
        'infix:<gt>' => ' gt ',

        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',

        'infix:<=~>' => ' =~ ',
        'infix:<!~>' => ' !~ ',
    );

    sub emit_perl5_args {
        my $self = $_[0];
        my $level = $_[1];
        return '' if !$self->{arguments};
        join(', ', map( $_->emit_perl5($level), @{$self->{arguments}} ))
    }

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
            return '(' . $self->{code}->emit_perl5($level+1) . ')->(' . $self->emit_perl5_args($level+1) . ')';
        }

        if (exists $op_infix_perl5{$code}) {
            return '(' 
                . join( $op_infix_perl5{$code},
                        (map( $_->emit_perl5($level+1), @{$self->{arguments}} ))
                      )
                . ')'
        }
        if (exists $op_prefix_perl5{$code}) {
            return $op_prefix_perl5{$code} . '(' . $self->emit_perl5_args($level+1) . ')'
        }

        if ($self->{code} eq 'p5:s') {
            return 's!' . $self->{arguments}->[0]->{buf}   # emit_perl5($level+1) 
                 .  '!' . $self->{arguments}->[1]->{buf}   # emit_perl5($level+1)
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
                        $s .= $ast->emit_perl5($level+1);  # variable name
                    }
                }
            }

            return 'm!' . $s . '!' . $self->{arguments}->[1];
        }
        if ($self->{code} eq 'p5:tr') {
            return 'tr!' . $self->{arguments}->[0]->{buf}   # emit_perl5($level+1) 
                 .   '!' . $self->{arguments}->[1]->{buf}   # emit_perl5($level+1)
                 .   '!';
        }

        if ($code eq '__PACKAGE__') {
            return '"' . $Perlito5::PKG_NAME . '"';
        }
        if ($self->{code} eq 'package')    { return 'package ' . $self->{namespace} }

        if ($code eq 'map')       {    

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                return "map {\n"
                .   join(";\n", map { Perlito5::Perl5::tab($level+1) . $_->emit_perl5( $level + 1 ) } @{$self->{special_arg}{stmts}} ) . "\n"
                . Perlito5::Perl5::tab($level) . "} "
    
                . $self->emit_perl5_args($level+1);
            }

            return 'map(' . $self->emit_perl5_args($level+1) . ')'
        }

        if ($code eq 'infix:<x>')  { 
            return 'join("", ' . join(' x ', map( $_->emit_perl5($level+1), @{$self->{arguments}} ))  . ')'
        }
        if ($code eq 'infix:<=>>')  { 
            return Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_perl5($level)  . ', ' 
                . $self->{arguments}[1]->emit_perl5($level)
        }

        if ( $code eq 'circumfix:<[ ]>' ) { return '[' . $self->emit_perl5_args( $level + 1 ) . ']' }
        if ( $code eq 'circumfix:<{ }>' ) { return '{' . $self->emit_perl5_args( $level + 1 ) . '}' }
        if ( $code eq 'circumfix:<( )>' ) { return '(' . $self->emit_perl5_args( $level + 1 ) . ')' }
        if ($code eq 'prefix:<\\>') { 
            # TODO - \(@a) vs. \@a
            return '\\' . $self->{arguments}[0]->emit_perl5($level+1) . ''
        }
        if ( $code eq 'prefix:<$>' )  { return '${' . $self->emit_perl5_args($level+1) . '}' }
        if ( $code eq 'prefix:<@>' )  { return '@{' . $self->emit_perl5_args($level+1) . '}' }
        if ( $code eq 'prefix:<%>' )  { return '%{' . $self->emit_perl5_args($level+1) . '}' }
        if ( $code eq 'prefix:<&>' )  { return '&{' . $self->emit_perl5_args($level+1) . '}' }
        if ( $code eq 'prefix:<*>' )  { return '*{' . $self->emit_perl5_args($level+1) . '}' }
        if ( $code eq 'prefix:<$#>' ) { return '$#{' . $self->emit_perl5_args($level+1) . '}' }

        if ( $code eq 'postfix:<++>' ) { return '(' . $self->emit_perl5_args($level+1) . ')++' }
        if ( $code eq 'postfix:<-->' ) { return '(' . $self->emit_perl5_args($level+1) . ')--' }

        if ($code eq 'infix:<..>') { return '('  . join(' .. ', map( $_->emit_perl5($level+1), @{$self->{arguments}} ))  . ")" }

        if ($code eq 'ternary:<? :>') {
            return '('  . $self->{arguments}->[0]->emit_perl5($level+1)
                . ' ? ' . $self->{arguments}->[1]->emit_perl5($level+1)
                . ' : ' . $self->{arguments}->[2]->emit_perl5($level+1)
                .  ')'
        }

        if ($code eq 'infix:<=>') {
            return emit_perl5_bind( $self->{arguments}->[0], $self->{arguments}->[1], $level );
        }

        if ($code eq 'require') {
            return 'Perlito5::Grammar::Use::require(' . $self->{arguments}[0]->emit_perl5($level+1) . ')'
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
                my $do = $arg->simplify->block;
                return "eval {\n" 
                    .  join(";\n", map( defined($_) && Perlito5::Perl5::tab($level+1) . $_->emit_perl5( $level + 1 ), @$do )) . "\n"
                    . Perlito5::Perl5::tab($level) . "}"
            }
            return 'eval ' . $self->emit_perl5_args($level+1);
        }

        if ($code eq 'return') {
            return 'return ' . $self->emit_perl5_args($level+1);
        }

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            return $code;
        }
        $code . '(' . $self->emit_perl5_args($level+1) . ')';
    }

    sub emit_perl5_bind {
        my $parameters = shift;
        my $arguments = shift;
        my $level = shift;

        if ($parameters->isa( 'Perlito5::AST::Call' )) {
            # $a->{3} = 4
            # $a->[3] = 4
            if  (  $parameters->method eq 'postcircumfix:<{ }>'
                || $parameters->method eq 'postcircumfix:<[ ]>'
                )
            {
                return $parameters->emit_perl5($level+1) . ' = ' . $arguments->emit_perl5($level+1);
            }
        }
        $parameters->emit_perl5($level+1) . ' = ' . $arguments->emit_perl5($level+1);
    }
}

package Perlito5::AST::If;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        return 'if (' . $self->{cond}->emit_perl5($level+1) . ") "
             .  ($self->{body}
                ? Perlito5::Perl5::emit_perl5_block($self->{body}->stmts, $level)
                : '{ }'
                )
             .  ($self->{otherwise} && scalar(@{ $self->{otherwise}->stmts })
                ?  ( "\n"
                    . Perlito5::Perl5::tab($level) . "else " 
                    . Perlito5::Perl5::emit_perl5_block($self->{otherwise}->stmts, $level)
                    )
                : ''
                );
    }
}

package Perlito5::AST::When;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        return 'when (' . $self->{cond}->emit_perl5($level+1) . ") "
             .  ($self->{body}
                ? Perlito5::Perl5::emit_perl5_block($self->{body}->stmts, $level)
                : '{ }'
                );
    }
}


package Perlito5::AST::While;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $self->{cond};
           'for ( '
        .  ( $self->{init}     ? $self->{init}->emit_perl5($level+1)           . '; ' : '; ' )
        .  ( $cond             ? $cond->emit_perl5($level+1)                   . '; ' : '; ' )
        .  ( $self->{continue} ? $self->{continue}->emit_perl5($level+1)       . ' '  : ' '  )
        .  ') ' . Perlito5::Perl5::emit_perl5_block($self->{body}->stmts, $level);
    }
}

package Perlito5::AST::For;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond;
        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            $cond =
               ( $self->{cond}[0] ? $self->{cond}[0]->emit_javascript($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[1] ? $self->{cond}[1]->emit_javascript($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[2] ? $self->{cond}[2]->emit_javascript($level + 1) . ' '   : ' '  )
        }
        else {
            $cond = $self->{cond}->emit_perl5($level+1)
        }

        my $sig = '';
        my $sig_ast = $self->{body}->sig();
        if (!$sig_ast) {
            # $_
        }
        elsif ($sig_ast->{decl}) {
            $sig = $sig_ast->{decl} . ' ' . $sig_ast->{type} . ' ' . $sig_ast->{var}->emit_perl5($level+1) . ' ';
        }
        else {
            $sig = $sig_ast->emit_perl5($level+1) . ' ';
        }
        return 'for ' . $sig . '(' . $cond . ') ' . Perlito5::Perl5::emit_perl5_block($self->{body}->stmts, $level);
    }
}

package Perlito5::AST::Decl;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $str = $self->{decl} . ' '
                . ($self->{type} ? $self->{type} . ' ' : '')
                . $self->{var}->emit_perl5($level+1);
        return $str;
    }
}

package Perlito5::AST::Sub;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my @parts;
        push @parts, $self->{namespace} . "::" . $self->{name}
            if $self->{name};
        push @parts, '(' . $self->{sig} . ')'
            if defined $self->{sig};
        push @parts, Perlito5::Perl5::emit_perl5_block($self->{block}, $level)
            if defined $self->{block};
        join(' ', 'sub', @parts);
    }
}

package Perlito5::AST::Do;
{
    sub emit_perl5 {
        my $self = $_[0];
        my $level = $_[1];
        
        my $block = $self->simplify->block;
        "(do " . Perlito5::Perl5::emit_perl5_block($block, $level) . ")";
    }
}

package Perlito5::AST::Use;
{
    sub emit_perl5 {
        my $self = shift;
        my $level = shift;
        Perlito5::Grammar::Use::emit_time_eval($self);

        return "\n"
            . Perlito5::Perl5::tab($level) . "# " . $self->{code} . " " . $self->{mod} . "\n"
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

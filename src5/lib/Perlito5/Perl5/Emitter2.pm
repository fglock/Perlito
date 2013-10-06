use v5;

use Perlito5::AST;

package Perlito5::Perl5;
{
    sub escape_string {
        return Perlito5::Dumper::escape_string($_[0]);
    }
    sub emit_perl5_2_block {
        my $block = $_[0];
        return [ 'block', 
                 map { defined($_) && $_->emit_perl5_2() } @$block
               ];
    }
}

package Perlito5::AST::CompUnit;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        return ( [ stmt => [ keyword => 'package'], [ bareword => $self->{name} ] ],
                 map { defined($_) && $_->emit_perl5_2() } @{$self->{body}}
               );
    }
    sub emit_perl5_2_program {
        my $comp_units = $_[0];
        return  map { $_->emit_perl5_2() } @$comp_units;
    }
}

package Perlito5::AST::Val::Int;
{
    sub emit_perl5_2 {
        my $self  = $_[0];
        $self->{int};
    }
}

package Perlito5::AST::Val::Num;
{
    sub emit_perl5_2 {
        my $self  = $_[0];
        $self->{num};
    }
}

package Perlito5::AST::Val::Buf;
{
    sub emit_perl5_2 {
        my $self  = $_[0];
        Perlito5::Perl5::escape_string( $self->{buf} );
    }
}

package Perlito5::AST::Lit::Block;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        # TODO - emit "continue" block
        Perlito5::Perl5::emit_perl5_2_block($self->{stmts});
    }
}

package Perlito5::AST::Index;
{
    sub emit_perl5_2 {
        my $self = $_[0];

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = $self->{obj};
            return $v->emit_perl5_2() . '[' . $self->{index_exp}->emit_perl5_2() . ']';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return $self->{obj}{arguments}[0]->emit_perl5_2() . '->[' . $self->{index_exp}->emit_perl5_2() . ']';
        }
        $self->{obj}->emit_perl5_2() . '->[' . $self->{index_exp}->emit_perl5_2() . ']';
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_perl5_2 {
        my $self = $_[0];

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            my $v = $self->{obj};
            return $v->emit_perl5_2() . '{' . $self->autoquote($self->{index_exp})->emit_perl5_2() . '}';
        }

        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return $self->{obj}{arguments}[0]->emit_perl5_2() . '->{' . $self->autoquote($self->{index_exp})->emit_perl5_2() . '}';
        }

        $self->{obj}->emit_perl5_2() . '->{' . $self->autoquote($self->{index_exp})->emit_perl5_2() . '}';
    }
}

package Perlito5::AST::Var;
{
    sub emit_perl5_2 {
        my $self = $_[0];

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
    sub emit_perl5_2 {
        my $self = $_[0];
        return [ bareword => $self->{name} ];
    }
}

package Perlito5::AST::Call;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        
        my $invocant = $self->{invocant}->emit_perl5_2();
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return $invocant . '->[' . $self->{arguments}->emit_perl5_2() . ']'
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return $invocant . '->{' . Perlito5::AST::Lookup->autoquote($self->{arguments})->emit_perl5_2() . '}'
        }
        my $meth = $self->{method};
        if  ($meth eq 'postcircumfix:<( )>')  {
            if (  ref($self->{invocant}) eq 'Perlito5::AST::Var'
               && $self->{invocant}{sigil} eq '&'
               )
            {
                #  &subr(args)
                return $invocant . '(' . join(', ', map( $_->emit_perl5_2(), @{$self->{arguments}} )) . ')';
            }
            $meth = '';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_perl5_2();
        }
        $invocant . '->' . $meth . '(' . join(', ', map( $_->emit_perl5_2(), @{$self->{arguments}} )) . ')';
    }
}

package Perlito5::AST::Apply;
{

    sub emit_perl5_2_args {
        my $self = $_[0];
        return () if !$self->{arguments};
        return map { $_->emit_perl5_2() } @{$self->{arguments}};
    }

    sub emit_perl5_2 {
        my $self = $_[0];
        
        if ( $Perlito5::Perl5::PrettyPrinter::op{ $self->{code} } ) {
            return [ op => $self->{code}, $self->emit_perl5_2_args() ];
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        my $code = $ns . $self->{code};

        if (ref $code ne '') {
            return '(' . $self->{code}->emit_perl5_2() . ')->(' . $self->emit_perl5_2_args() . ')';
        }

        if ($self->{code} eq 'p5:s') {
            return 's!' . $self->{arguments}->[0]->{buf}   # emit_perl5_2() 
                 .  '!' . $self->{arguments}->[1]->{buf}   # emit_perl5_2()
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
                        $s .= $ast->emit_perl5_2();  # variable name
                    }
                }
            }

            return 'm!' . $s . '!' . $self->{arguments}->[1];
        }
        if ($self->{code} eq 'p5:tr') {
            return 'tr!' . $self->{arguments}->[0]->{buf}   # emit_perl5_2() 
                 .   '!' . $self->{arguments}->[1]->{buf}   # emit_perl5_2()
                 .   '!';
        }

        if ($code eq '__PACKAGE__') {
            return '"' . $Perlito5::PKG_NAME . '"';
        }
        if ($self->{code} eq 'package')    { return 'package ' . $self->{namespace} }

        if ($code eq 'map' || $code eq 'grep' || $code eq 'sort') {    

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                return "$code {\n"
                .   join(";\n", map { $_->emit_perl5_2() } @{$self->{special_arg}{stmts}} ) . "\n"
                . "} "
    
                . $self->emit_perl5_2_args();
            }

            return "$code(" . $self->emit_perl5_2_args() . ')'
        }

        if ( $code eq 'prefix:<$>' )  { return '${' . $self->emit_perl5_2_args() . '}' }
        if ( $code eq 'prefix:<@>' )  { return '@{' . $self->emit_perl5_2_args() . '}' }
        if ( $code eq 'prefix:<%>' )  { return '%{' . $self->emit_perl5_2_args() . '}' }
        if ( $code eq 'prefix:<&>' )  { return '&{' . $self->emit_perl5_2_args() . '}' }
        if ( $code eq 'prefix:<*>' )  { return '*{' . $self->emit_perl5_2_args() . '}' }
        if ( $code eq 'prefix:<$#>' ) { return '$#{' . $self->emit_perl5_2_args() . '}' }

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            return [ bareword => $code ];
        }
        $code . '(' . $self->emit_perl5_2_args() . ')';
    }
}

package Perlito5::AST::If;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Lit::Block') {
            return [ stmt_modifier => $self->{body}->emit_perl5_2(),
                                      [ stmt => 'if', $self->{cond}->emit_perl5_2() ] ];
        }
        if ($self->{otherwise} && ref($self->{otherwise}) ne 'Perlito5::AST::Lit::Block') {
            return [ stmt_modifier => $self->{otherwise}->emit_perl5_2(),
                                      [ stmt => 'unless', $self->{cond}->emit_perl5_2() ] ];
        }

        return 'if (' . $self->{cond}->emit_perl5_2() . ") "
             .  ($self->{body}
                ? Perlito5::Perl5::emit_perl5_2_block($self->{body}->stmts)
                : '{ }'
                )
             .  ($self->{otherwise} && scalar(@{ $self->{otherwise}->stmts })
                ?  ( "\n"
                    . "else " 
                    . Perlito5::Perl5::emit_perl5_2_block($self->{otherwise}->stmts)
                    )
                : ''
                );
    }
}

package Perlito5::AST::When;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        return 'when (' . $self->{cond}->emit_perl5_2() . ") "
             .  ($self->{body}
                ? Perlito5::Perl5::emit_perl5_2_block($self->{body}->stmts)
                : '{ }'
                );
    }
}


package Perlito5::AST::While;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Lit::Block') {
            return [ stmt_modifier => $self->{body}->emit_perl5_2(),
                                      [ stmt => 'while', $self->{cond}->emit_perl5_2() ] ];
        }

           'for ( '
        .  ( $self->{init}     ? $self->{init}->emit_perl5_2()           . '; ' : '; ' )
        .  ( $self->{cond}     ? $self->{cond}->emit_perl5_2()           . '; ' : '; ' )
        .  ( $self->{continue} ? $self->{continue}->emit_perl5_2()       . ' '  : ' '  )
        .  ') ' . Perlito5::Perl5::emit_perl5_2_block($self->{body}->stmts);
    }
}

package Perlito5::AST::For;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Lit::Block') {
            return [ stmt_modifier => $self->{body}->emit_perl5_2(),
                                      [ stmt => 'for', $self->{cond}->emit_perl5_2() ] ];
        }

        my $cond;
        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            $cond =
               ( $self->{cond}[0] ? $self->{cond}[0]->emit_javascript() . '; '  : '; ' )
            .  ( $self->{cond}[1] ? $self->{cond}[1]->emit_javascript() . '; '  : '; ' )
            .  ( $self->{cond}[2] ? $self->{cond}[2]->emit_javascript() . ' '   : ' '  )
        }
        else {
            $cond = $self->{cond}->emit_perl5_2()
        }

        my $sig = '';
        my $sig_ast = $self->{body}->sig();
        if (!$sig_ast) {
            # $_
        }
        elsif ($sig_ast->{decl}) {
            $sig = $sig_ast->{decl} . ' ' . $sig_ast->{type} . ' ' . $sig_ast->{var}->emit_perl5_2() . ' ';
        }
        else {
            $sig = $sig_ast->emit_perl5_2() . ' ';
        }
        return 'for ' . $sig . '(' . $cond . ') ' . Perlito5::Perl5::emit_perl5_2_block($self->{body}->stmts);
    }
}

package Perlito5::AST::Decl;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        return [ op => 'prefix:<' . $self->{decl} . '>', 
                 ($self->{type} ? $self->{type} : ()),
                 $self->{var}->emit_perl5_2()
               ];
    }
}

package Perlito5::AST::Sub;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        my @parts;
        push @parts, [ paren => '(', [ bareword => $self->{sig} ] ]
            if defined $self->{sig};
        push @parts, Perlito5::Perl5::emit_perl5_2_block($self->{block})
            if defined $self->{block};
        return [ op => 'prefix:<sub>', @parts ] if !$self->{name};
        return [ stmt => [ keyword => 'sub' ], [ bareword => $self->{namespace} . "::" . $self->{name} ], @parts ];
    }
}

package Perlito5::AST::Do;
{
    sub emit_perl5_2 {
        my $self = $_[0];
        my $block = $self->simplify->block;
        return [ op => 'prefix:<do>', Perlito5::Perl5::emit_perl5_2_block($block) ];
    }
}

package Perlito5::AST::Use;
{
    sub emit_perl5_2 {
        my $self = shift;
        Perlito5::Grammar::Use::emit_time_eval($self);
        return [ comment => "# " . $self->{code} . " " . $self->{mod} ];
    }
}

=begin

=head1 NAME

Perlito5::Perl5::Emit - Code generator for Perlito5-in-Perl5

=head1 SYNOPSIS

    $program->emit_perl5_2()  # generated Perl5 code

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

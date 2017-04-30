use v5;

use Perlito5::AST;
use strict;

package Perlito5::AST::CompUnit;
{
    sub get_captures { () }
}

package Perlito5::AST::Int;
{
    sub get_captures { () }
}

package Perlito5::AST::Num;
{
    sub get_captures { () }
}

package Perlito5::AST::Buf;
{
    sub get_captures { () }
}

package Perlito5::AST::Block;
{
    sub get_captures {
        my ($self) = @_;
        return @{
            $self->{_get_captures} //= do {
                my @var;
                for my $stmt ( @{ $self->{stmts} } ) {
                    push @var, $stmt->get_captures();
                }
                \@var;
              }
        };
    }
}

package Perlito5::AST::Index;
{
    sub get_captures {
        my $self      = shift;
        my @var;
        push @var, $self->{obj}->get_captures();
        push @var, $self->{index_exp}->get_captures();
        return @var;
    }
}

package Perlito5::AST::Lookup;
{
    sub get_captures {
        my $self      = shift;
        my @var;
        push @var, $self->{obj}->get_captures();
        push @var, $self->{index_exp}->get_captures();
        return @var;
    }
}

package Perlito5::AST::Var;
{
    sub get_captures {
        my $self = shift;
        return ($self); 
    }
}

package Perlito5::AST::Decl;
{
    sub get_captures {
        # non-free variables are not captured
        return { dont => $_[0]{var}{_id} } 
    }
}

package Perlito5::AST::Call;
{
    sub get_captures {
        my $self      = shift;
        my @var;
        push @var, $self->{method}->get_captures() if ref($self->{method});
        push @var, $self->{invocant}->get_captures();
        my $args = $self->{arguments};
        if ($args) {
            if (ref($args) eq "ARRAY") {
                push @var, map { $_->get_captures() } @$args;
            }
            else {
                push @var, $args->get_captures();
            }
        }
        return @var;
    }
}

package Perlito5::AST::Apply;
{
    sub get_captures {
        my $self      = shift;
        my $code = $self->{code};
        my @var;
        push @var, $code->get_captures()
            if ref($code);
        push @var, map  { $_->get_captures() }
                        @{ $self->{arguments} }
                if $self->{arguments};
        push @var, $self->{special_arg}->get_captures()
            if ref($self->{special_arg});
        if ($code eq 'my' || $code eq 'our' || $code eq 'state') {
            push @var, ( map {     ref($_) eq 'Perlito5::AST::Var'
                             ? ( { dont => $_->{_id} } )
                             : ()
                         }
                         @{ $self->{arguments} }
                   );
        }
        return @var;
    }
}

package Perlito5::AST::If;
{
    sub get_captures {
        my $self      = shift;
        my @var;
        push @var, $self->{cond}->get_captures();
        push @var, $self->{body}->get_captures();
        push @var, $self->{otherwise}->get_captures();
        return @var;
    }
}


package Perlito5::AST::When;
{
    sub get_captures {
        my $self      = shift;
        my @var;
        push @var, $self->{cond}->get_captures();
        push @var, $self->{body}->get_captures();
        return @var;
    }
}


package Perlito5::AST::While;
{
    sub get_captures {
        my $self      = shift;
        my @var;
        push @var, $self->{cond}->get_captures();
        push @var, $self->{body}->get_captures();
        return @var;
    }
}

package Perlito5::AST::For;
{
    sub get_captures {
        my $self      = shift;
        my @var;
        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Block'
            ? [ $self->{body} ]
            : $self->{body}{stmts};
        push @var, map { $_->get_captures() }
                   grep { defined }
                @$body,
                $self->{topic},
                ( ref( $self->{cond} ) eq 'ARRAY'
                   ? @{ $self->{cond} }
                   : $self->{cond} );
        return @var;
    }
}

package Perlito5::AST::Sub;
{
    sub get_captures {
        my $self      = shift;
        return if !$self->{block};  # predeclaration
        return $self->{block}->get_captures()
    }
}

1;

=begin

=head1 NAME

Perlito5::AST::Captures

=head1 SYNOPSIS

    $ast->get_captures()

=head1 DESCRIPTION

This module generates a list of captured variables

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2015 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

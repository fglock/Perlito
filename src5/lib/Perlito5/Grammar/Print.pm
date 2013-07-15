
package Perlito5::Grammar::Print;

use strict;

Perlito5::Precedence::add_term( 'print'  => sub { Perlito5::Grammar::Print->term_print($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'say'    => sub { Perlito5::Grammar::Print->term_print($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'exec'   => sub { Perlito5::Grammar::Print->term_print($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'system' => sub { Perlito5::Grammar::Print->term_print($_[0], $_[1]) } );

token print_decl { 'print' | 'say' | 'exec' | 'system' };

token the_object {
    # TODO

    '$' ...
            [
                <two_terms_in_a_row>
                    { ok ...
                    }
            |
                <.Perlito5::Grammar::Space.ws>
                '+'
                <!Perlito5::Grammar::Space.ws>
                    { ok ...
                    }
            |
                <.Perlito5::Grammar::Space.opt_ws>
                <.Perlito5::Precedence::infix>
                    { return;   # abort because it looks like an infix
                    }
            |
                <Perlito5::Expression.list_parse>
                    {   # abort because there is no list 
                    ...
                    }
            |
                    { ok ... 
                    }
            ]

    '{'
            block ...

    bareword
            {   # abort because there is a subroutine with this name
            }

}

sub print_ast {
    my ($decl, $the_object, $expr) = @_;
    Perlito5::AST::Apply->new( 
        'namespace' => 'Perlito5',
        'code'      => $decl,
        'arguments' => [
            $the_object,
            @$expr,
        ],
    )
}

token term_print {
    <print_decl> 
    <.Perlito5::Grammar::Space.opt_ws>
    [
        '('
            <the_object>
            <Perlito5::Expression.paren_parse>
        ')'

        { $MATCH->{capture} = [
                'term',
                print_ast(
                    Perlito5::Match::flat($MATCH->{'print_decl'}),
                    Perlito5::Match::flat($MATCH->{'the_object'}),
                    Perlito5::Match::flat($MATCH->{'Perlito5::Expression.paren_parse'}),
                ),
            ]
        }
    |
        <the_object>
        <Perlito5::Expression.list_parse>

        { $MATCH->{capture} = [
                'term',
                print_ast(
                    Perlito5::Match::flat($MATCH->{'print_decl'}),
                    Perlito5::Match::flat($MATCH->{'the_object'}),
                    Perlito5::Match::flat($MATCH->{'Perlito5::Expression.list_parse'}),
                ),
            ]
        }
    ]
}

1;

=begin

=head1 NAME

Perlito5::Grammar::Print - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_print($str)

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2013 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


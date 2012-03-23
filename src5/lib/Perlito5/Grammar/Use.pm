
package Perlito5::Grammar::Use;

use Perlito5::Precedence;
use Perlito5::Grammar;

Perlito5::Precedence::add_term( 'no'  => sub { Perlito5::Grammar::Use->term_use($_[0], $_[1]) } );
Perlito5::Precedence::add_term( 'use' => sub { Perlito5::Grammar::Use->term_use($_[0], $_[1]) } );


token use_decl { 'use' | 'no' };

token term_use {
    <use_decl> <.Perlito5::Grammar.ws>
        <Perlito5::Grammar.full_ident>  [ - <Perlito5::Grammar.ident> ]? <Perlito5::Expression.list_parse>
        {
            my $ast = Perlito5::AST::Use->new(
                    code => $MATCH->{"use_decl"}->flat(),
                    mod  => $MATCH->{"Perlito5::Grammar.full_ident"}->flat()
                );

            parse_time_eval($ast);

            $MATCH->{"capture"} = [ 'term', $ast ];
        }
};

sub parse_time_eval {
    my $self = shift;
    if ($self->mod eq 'strict') {
        if ($self->code eq 'use') {
            Perlito5::strict->import();
        }
        elsif ($self->code eq 'no') {
            Perlito5::strict->unimport();
        }
    }
}

sub emit_time_eval {
    my $self = shift;
    if ($self->mod eq 'strict') {
        if ($self->code eq 'use') {
            Perlito5::strict->import();
        }
        elsif ($self->code eq 'no') {
            Perlito5::strict->unimport();
        }
    }
}


1;

=begin

=head1 NAME

Perlito5::Grammar::Use - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_use($str)

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


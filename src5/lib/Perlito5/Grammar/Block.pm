
package Perlito5::Grammar::Block;

use Perlito5::Expression;

Perlito5::Expression::add_statement( '{'     => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );
Perlito5::Expression::add_statement( 'BEGIN' => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );


sub term_block {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

    if ( substr($str, $pos, 1) eq '{' ) {
        # do we recognize a bare block in this position?
        # warn "maybe bareblock at $pos";
        my $m = Perlito5::Expression->term_curly($str, $pos);
        if ($m) {
            my $v = $m->flat();

            # TODO - this is not recognized as a statement: { 123 => 4;}
            # TODO - this is not recognized as a syntax error: { 123 => 4 }{2}

            $v = Perlito5::AST::Lit::Block->new( stmts => $v->[2], sig => $v->[3] );
            $v = Perlito5::Expression::block_or_hash($v);

            if ( ref($v) eq 'Perlito5::AST::Lit::Block' ) {
                $m->{"capture"} = $v;
                return $m;
            }
        }
    }

    return 0;
}

1;

=begin

=head1 NAME

Perlito5::Grammar::Block - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_block($str)

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



package Perlito5::Grammar::Block;

use Perlito5::Expression;

Perlito5::Expression::add_statement( '{'     => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );
Perlito5::Expression::add_statement( 'BEGIN' => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );
Perlito5::Expression::add_statement( 'UNITCHECK' => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );
Perlito5::Expression::add_statement( 'CHECK' => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );
Perlito5::Expression::add_statement( 'INIT'  => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );
Perlito5::Expression::add_statement( 'END'   => sub { Perlito5::Grammar::Block->term_block($_[0], $_[1]) } );


sub term_block {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

    my $p = $pos;
    my $block_name;
    my $m_name = Perlito5::Grammar->ident( $str, $p );
    if ($m_name) {
        $p = $m_name->{to};
        $block_name = Perlito5::Match::flat($m_name);
    }

    my $ws = Perlito5::Grammar::Space->ws( $str, $p );
    if ( $ws ) {
        $p = $ws->{to};
    }

    if ( substr($str, $p, 1) eq '{' ) {
        # do we recognize a bare block in this position?
        # warn "maybe bareblock at $p";
        my $m = Perlito5::Expression->term_curly($str, $p);
        if ($m) {
            my $block_start = $p;
            $p = $m->{to};
            $ws = Perlito5::Grammar::Space->ws( $str, $p );
            if ( $ws ) {
                $p = $ws->{to};
            }
            my $continue = Perlito5::AST::Lit::Block->new(stmts => [] );
            my $has_continue = 0;
            if ( !$block_name && substr($str, $p, 8) eq 'continue' ) {
                # anonymous blocks can have a 'continue' block
                $p += 8;
                $ws = Perlito5::Grammar::Space->ws( $str, $p );
                if ( $ws ) {
                    $p = $ws->{to};
                }
                my $cont = Perlito5::Expression->term_curly($str, $p);
                die "syntax error" unless $cont;
                warn "continue!";

                $continue->{stmts} = $cont->{capture}[2];
                $has_continue = 1;
                $m->{to} = $cont->{to};
            }

            my $v = Perlito5::Match::flat($m);

            # TODO - this is not recognized as a statement: { 123 => 4;}
            # TODO - this is not recognized as a syntax error: { 123 => 4 }{2}

            $v = Perlito5::AST::Lit::Block->new( stmts => $v->[2], sig => $v->[3] );
            $v = Perlito5::Expression::block_or_hash($v)
                unless $has_continue || $block_name;

            if ( ref($v) eq 'Perlito5::AST::Lit::Block' ) {
                if ($block_name eq 'BEGIN') {
                    # say "BEGIN $block_start ", $m->{to}, "[", substr($str, $block_start, $m->{to} - $block_start), "]";
                    eval substr($str, $block_start, $m->{to} - $block_start);
                    $m->{capture} = 
                        Perlito5::AST::Apply->new(
                            code => 'undef',
                            namespace => '',
                            arguments => []
                        );
                }
                else {
                    $v->{name} = $block_name;
                    $m->{capture} = $v;
                    $m->{capture}{continue} = $continue;
                }
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


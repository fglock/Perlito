use v5;

package Perlito5::Match;

sub flat {
    my $self = $_[0];
    return $self->{capture} if defined( $self->{capture} );
    return join( "",
        @{ $self->{str} }[ $self->{from} .. $self->{to} - 1 ]
    );
}

1;

=begin

=head1 NAME

Perlito5::Match - Runtime for Perlito Perl5-in-JavaScript grammars

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2009, 2011, 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

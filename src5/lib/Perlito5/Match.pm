use v5;

package Perlito5::Match;

sub new {
    my $class = shift;
    return { @_ }
}

sub flat {
    my $self = $_[0];
    defined( $self->{'capture'} )
    ? $self->{'capture'}
    : substr( $self->{'str'}, $self->{'from'}, ( $self->{'to'} - $self->{'from'} ) )
}

1;

=begin

=head1 NAME

Perlito5::Match - Runtime for Perlito Perl5-in-Javascript grammars

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2009, 2011, 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

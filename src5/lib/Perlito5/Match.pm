use v5;

package Perlito5::Match;

sub new {
    my $class = shift;
    bless { @_ }, $class
}

sub from    { $_[0]->{'from'} }
sub to      { $_[0]->{'to'} }
sub str     { $_[0]->{'str'} }
sub capture { $_[0]->{'capture'} }

sub flat {
    my $self = $_[0];
    if ( defined( $self->{'capture'} ) ) {
        return $self->{'capture'};
    }
    return substr( $self->{'str'}, $self->{'from'}, ( $self->{'to'} - $self->{'from'} ) );
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

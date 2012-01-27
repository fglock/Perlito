use v5;

class Perlito5::Match {
    has $.from;
    has $.to;
    has $.str;
    has $.bool;
    has $.capture;

    sub flat {
        my $self = $_[0];

        if ($.bool) {
            if (defined($.capture)) {
                return $.capture;
            }
            return substr( $.str, $.from, ( $.to - $.from ) );
        }
        else {
            return '';
        }
    }

}

=begin

=head1 NAME

Perlito5::Javascript::Prelude - Runtime for Perlito Perl5-in-Javascript

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2009, 2011 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

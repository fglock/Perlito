use v5;

class Perlito5::Match {
    has $.from;
    has $.to;
    has $.str;
    has $.bool;
    has $.capture;

    sub scalar {
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

    sub string {
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

class Main {
    1;
}

=begin

=head1 NAME

Perlito5::Javascript::Prelude - Runtime for Perlito-in-Javascript

=head1 SYNOPSIS

=head1 DESCRIPTION

This module contains Perlito code for the Perlito-in-Lisp runtime.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2009, 2011 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

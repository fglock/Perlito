use v6;

class Perlito6::Match {
    has $.from;
    has $.to;
    has $.str;
    has $.bool;

    method scalar {
        substr( $.str, $.from, ( $.to - $.from ) );
    }
}

=begin

=head1 NAME 

Perlito6::Ruby::Prelude - Runtime for Perlito-in-Ruby

=head1 SYNOPSIS

=head1 DESCRIPTION

This module contains Perlito code for the Perlito-in-Ruby runtime.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2009, 2011, 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

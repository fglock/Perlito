package Perlito5::Grammar::Space;

use Perlito5::Precedence;

Perlito5::Precedence::add_term( '#'     => sub { Perlito5::Expression->term_space($_[0], $_[1]) } );
Perlito5::Precedence::add_term( chr(9)  => sub { Perlito5::Expression->term_space($_[0], $_[1]) } );
Perlito5::Precedence::add_term( chr(10) => sub { Perlito5::Expression->term_space($_[0], $_[1]) } );
Perlito5::Precedence::add_term( chr(12) => sub { Perlito5::Expression->term_space($_[0], $_[1]) } );
Perlito5::Precedence::add_term( chr(13) => sub { Perlito5::Expression->term_space($_[0], $_[1]) } );
Perlito5::Precedence::add_term( chr(32) => sub { Perlito5::Expression->term_space($_[0], $_[1]) } );

1;

=begin

=head1 NAME

Perlito5::Grammar::Space - Grammar for Perlito5 "whitespace"

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHOR

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

=head1 COPYRIGHT

Copyright 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


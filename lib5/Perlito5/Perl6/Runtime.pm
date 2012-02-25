
use v6;

module Perlito5::Perl6::Runtime {

sub ref ($obj) { #OK
    my $s = $obj.WHAT.perl;
    return 'ARRAY' if $s eq 'Array';
    return 'HASH'  if $s eq 'Hash';
    return ''      if $s eq 'Str';
    return ''      if $s eq 'Int';
    return ''      if $s eq 'Rat';
    # TODO - REF, CODE, ...
    return $s;
}

}

=begin

=head1 NAME

Perlito5::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the Perlito-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

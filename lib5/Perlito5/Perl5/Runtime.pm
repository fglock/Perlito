
use v5;

use Perlito5::Match;

package Perlito5::Grammar;
    sub space {
        # my $grammar = $_[0];
        bless {
            str  => $_[1],
            from => $_[2],
            to   => $_[2] + 1,
            bool => substr( $_[1], $_[2] ) =~ m/^([[:space:]])/ ? 1 : 0,
          },
          'Perlito5::Match';
    }
    sub digit {
        # my $grammar = $_[0];
        bless {
            str  => $_[1],
            from => $_[2],
            to   => $_[2] + 1,
            bool => substr( $_[1], $_[2] ) =~ m/^([[:digit:]])/ ? 1 : 0,
          },
          'Perlito5::Match';
    }
    sub word {
        # my $grammar = $_[0];
        bless {
            str  => $_[1],
            from => $_[2],
            to   => $_[2] + 1,
            bool => substr( $_[1], $_[2] ) =~ m/^([[:word:]])/ ? 1 : 0,
          },
          'Perlito5::Match';
    }

package IO;

    sub slurp {
        my $source_filename = shift;
        open FILE, $source_filename
          or die "Cannot read $source_filename\n";
        local $/ = undef;
        $source = <FILE>;
        close FILE;
        return $source;
    }

package Perlito5::Runtime;

    sub say   { print( @_, "\n" ) }

1;

__END__

=pod

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

=cut

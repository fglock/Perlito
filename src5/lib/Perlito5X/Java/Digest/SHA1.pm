package Digest::SHA1;
use MIME::Base64;
use strict;

use Exporter qw(import);
our @EXPORT = qw();
our @EXPORT_OK = qw(sha1 sha1_hex sha1_base64);

package Java::MessageDigest { import => "java.security.MessageDigest" }
package Java::BigInteger    { import => "java.math.BigInteger" }
package Java::Base64        { import => "java.util.Base64" }
package String {};

sub sha1_hex {
    eval {
        my $arg = shift;
        my String $s = $arg;
        my $result = Java::BigInteger->new(1, Java::MessageDigest->getInstance("SHA1")->digest($s->getBytes("UTF-8")))->toString(16);
        return $result;
    }
    or die $@;
}

sub sha1 {
    eval {
        my $arg = shift;
        my String $s = $arg;
        my $result = String->new(Java::MessageDigest->getInstance("SHA1")->digest($s->getBytes("UTF-8")));
        return $result;
    }
    or die $@;
}

sub sha1_base64 {
    eval {
        my $arg = shift;
        my String $s = $arg;
        my $result =
            Java::Base64->getMimeEncoder()->encodeToString( Java::MessageDigest->getInstance("SHA1")->digest($s->getBytes("UTF-8")));
        $result =~ s/=+$//;
        return $result;
    }
    or die $@;
}

1;

__END__

# Test:
# 
# perl perlito5.pl -Isrc5/lib -Isrc5 -Cjava -e ' use Digest::SHA1; print Digest::SHA1::sha1("abc"), "\n"; '  > Main.java ; javac Main.java ; java Main
#


=head1 COPYRIGHT

The original Digest::SHA1 module is

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

 Copyright 1999-2004 Gisle Aas.
 Copyright 1997 Uwe Hollerbach.

=head1 AUTHORS

Peter C. Gutmann,
Uwe Hollerbach <uh@alumni.caltech.edu>,
Gisle Aas <gisle@aas.no>

=cut


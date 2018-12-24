package Digest::MD5;
use MIME::Base64;
use strict;

use Exporter qw(import);
our @EXPORT = qw();
our @EXPORT_OK = qw(md5 md5_hex md5_base64);

package Java::MessageDigest { import => "java.security.MessageDigest" }
package Java::BigInteger    { import => "java.math.BigInteger" }
package Java::Base64        { import => "java.util.Base64" }
package String {};

sub md5_hex {
    eval {
        my $arg = shift;
        my String $s = $arg;
        my $result = Java::BigInteger->new(1, Java::MessageDigest->getInstance("MD5")->digest($s->getBytes("UTF-8")))->toString(16);
        return $result;
    }
    or die $@;
}

sub md5 {
    eval {
        my $arg = shift;
        my String $s = $arg;
        my $result = String->new(Java::MessageDigest->getInstance("MD5")->digest($s->getBytes("UTF-8")));
        return $result;
    }
    or die $@;
}

sub md5_base64 {
    eval {
        my $arg = shift;
        my String $s = $arg;
        my $result =
            Java::Base64->getMimeEncoder()->encodeToString( Java::MessageDigest->getInstance("MD5")->digest($s->getBytes("UTF-8")));
        $result =~ s/=+$//;
        return $result;
    }
    or die $@;
}

1;

__END__

# Test:
# 
# perl perlito5.pl -Isrc5/lib -Isrc5 -Cjava -e ' use Digest::MD5; print Digest::MD5::md5("abc"), "\n"; '  > Main.java ; javac Main.java ; java Main
#


=head1 COPYRIGHT

The original Digest::MD5 module is

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

 Copyright 1998-2003 Gisle Aas.
 Copyright 1995-1996 Neil Winton.
 Copyright 1991-1992 RSA Data Security, Inc.

The MD5 algorithm is defined in RFC 1321. This implementation is
derived from the reference C code in RFC 1321 which is covered by
the following copyright statement:

=over 4

=item

Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software.

=back

This copyright does not prohibit distribution of any version of Perl
containing this extension under the terms of the GNU or Artistic
licenses.

=head1 AUTHORS

The original C<MD5> interface was written by Neil Winton
(C<N.Winton@axion.bt.co.uk>).

The C<Digest::MD5> module is written by Gisle Aas <gisle@ActiveState.com>.

=cut

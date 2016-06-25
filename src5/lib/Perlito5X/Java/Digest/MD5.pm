package Digest::MD5;
use strict;

use Exporter qw(import);
our @EXPORT = qw();
our @EXPORT_OK = qw();

package Java::MessageDigest { import => "java.security.MessageDigest" }
package Java::BigInteger    { import => "java.math.BigInteger" }
package String {};

sub md5_hex {
    eval {
        my String $s = shift->toString();
        my $result = Java::BigInteger->new(1, Java::MessageDigest->getInstance("MD5")->digest($s->getBytes("UTF-8")))->toString(16);
        return $result;
    }
    or die $@;
}

1;

__END__

# Test:
# 
# perl perlito5.pl -Isrc5/lib -I. -It -I src5/lib/Perlito5X/Java  -Cjava -e ' use Digest::MD5; print Digest::MD5::md5("abc"), "\n"; '  > Main.java ; javac Main.java ; java Main
#




package MIME::Base64;
use strict;

package Java::DatatypeConverter { import => "javax.xml.bind.DatatypeConverter" }

# encode_base64( $bytes )
# encode_base64( $bytes, $eol );

sub encode_base64 {
    my $s = shift;
    my $eol = shift // "\n";
    my $result =
      Java::DatatypeConverter->printBase64Binary( $s->toString()->getBytes() );
    my @out;
    while ($result) {
        push @out, substr($result, 0, 76);
        $result = substr($result, 76);
    }
    return join($eol, @out) . $eol;
}

1;

__END__

package MIME::Base64;
use strict;

# Java 8 only
package Java::Base64 { import => "java.util.Base64" }

# encode_base64( $bytes )
# encode_base64( $bytes, $eol );

sub encode_base64 {
    my $s = shift;
    my $eol = shift // "\n";
    my $result =
      Java::Base64->getEncoder()->encodeToString( $s->toString()->getBytes() );
    my @out;
    while ($result) {
        push @out, substr($result, 0, 76);
        $result = substr($result, 76);
    }
    return join($eol, @out) . $eol;
}

1;

__END__

Test:

perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' use MIME::Base64; print MIME::Base64::encode_base64("abc"), "\n"; '  > Main.java
javac Main.java
java Main

Java docs:

// encode with padding
String encoded = Base64.getEncoder().encodeToString(someByteArray);

// encode without padding
String encoded = Base64.getEncoder().withoutPadding().encodeToString(someByteArray);

// decode a String
byte [] barr = Base64.getDecoder().decode(encoded);


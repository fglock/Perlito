package MIME::Base64;

package Java::Base64 { import => "java.util.Base64" }

# encode_base64( $bytes )
# encode_base64( $bytes, $eol );

sub encode_base64 {
    my $s = shift;
    my $result = Java::Base64->getEncoder()->encodeToString($s->toString()->getBytes());
    return $result;
}

1;

__END__

// encode with padding
String encoded = Base64.getEncoder().encodeToString(someByteArray);

// encode without padding
String encoded = Base64.getEncoder().withoutPadding().encodeToString(someByteArray);

// decode a String
byte [] barr = Base64.getDecoder().decode(encoded);


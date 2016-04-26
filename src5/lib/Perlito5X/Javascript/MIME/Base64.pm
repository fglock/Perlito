package MIME::Base64;
use strict;

# See: https://developer.mozilla.org/en/docs/Web/API/WindowBase64/Base64_encoding_and_decoding#Solution_.232_.E2.80.93_rewriting_atob()_and_btoa()_using_TypedArrays_and_UTF-8

JS::inline(q!
    var btoa = require('btoa');
    var atob = require('atob');
!);

# encode_base64( $bytes )
# encode_base64( $bytes, $eol );
#    Encode data by calling the encode_base64() function.  The first argument is the byte string to encode.
#    The second argument is the line-ending sequence to use.  It is optional and defaults to "\n".  The
#    returned encoded string is broken into lines of no more than 76 characters each and it will end with
#    $eol unless it is empty.  Pass an empty string as second argument if you do not want the encoded string
#    to be broken into lines.
#   
#    The function will croak with "Wide character in subroutine entry" if $bytes contains characters with
#    code above 255.  The base64 encoding is only defined for single-byte characters.  Use the Encode module
#    to select the byte encoding you want.
sub encode_base64 {
    my $s = shift;
    my $eol = shift // "\n";
    my $result = JS::inline("btoa(v_s)");
    my @out;
    while ($result) {
        push @out, substr($result, 0, 76);
        $result = substr($result, 76);
    }
    return join($eol, @out) . $eol;
}

# decode_base64( $str )
#    Decode a base64 string by calling the decode_base64() function.  This function takes a single argument
#    which is the string to decode and returns the decoded data.
#
#    Any character not part of the 65-character base64 subset is silently ignored.  Characters occurring
#    after a '=' padding character are never decoded.
sub decode_base64 {
    my $s = shift;
    my $end = index($s, "=");
    if ($end >= 0) {
        $s = substr($s, 0, $end);
    }
    my $result = JS::inline("atob(v_s)");
    return $result;
}

# plain-Perl code from MIME::Base64
## TODO - use original code with glob assignment:
# *encode = \&encode_base64;
# *decode = \&decode_base64;

sub encode { encode_base64(@_) }
sub decode { decode_base64(@_) }

sub encode_base64url {
    my $e = encode_base64(shift, "");
    $e =~ s/=+\z//;
    $e =~ tr[+/][-_];
    return $e;
}

sub decode_base64url {
    my $s = shift;
    $s =~ tr[-_][+/];
    $s .= '=' while length($s) % 4;
    return decode_base64($s);
}

1;

__END__

Test:

node perlito5.js -Isrc5/lib -Cjs -e ' use MIME::Base64; print MIME::Base64::encode_base64("abc"), "\n"; '



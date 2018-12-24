package MIME::Base64;
use strict;

use Exporter qw(import);
our @EXPORT = qw(encode_base64 decode_base64);
our @EXPORT_OK = qw(encode_base64url decode_base64url encoded_base64_length decoded_base64_length);

# Java 8 only
# package Java::Base64 { import => "java.util.Base64" }

package Java::Base64 { import => "java.util.Base64" }
package String {};

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
    my String $str = $s;
    my $result =
      Java::Base64->getMimeEncoder()->encodeToString( $str->getBytes() );
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
    my String $str = $s;
    my $result =
      String->new( Java::Base64->getMimeDecoder()->decode( $str ) );
    return $result;
}

# plain-Perl code from MIME::Base64
*encode = \&encode_base64;
*decode = \&decode_base64;

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


sub encoded_base64_length { length(encode_base64(@_)) }
sub decoded_base64_length { length(decode_base64(@_)) }

1;

__END__

# Test:
# 
# perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' use MIME::Base64; print MIME::Base64::encode_base64("abc"), "\n"; '  > Main.java ; javac Main.java ; java Main
# 

=head1 COPYRIGHT

The original MIME::Base64 module is

Copyright 1995-1999, 2001-2004, 2010 Gisle Aas.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

Distantly based on LWP::Base64 written by Martijn Koster
<m.koster@nexor.co.uk> and Joerg Reichelt <j.reichelt@nexor.co.uk> and
code posted to comp.lang.perl <3pd2lp$6gf@wsinti07.win.tue.nl> by Hans
Mulder <hansm@wsinti07.win.tue.nl>

The XS implementation uses code from metamail.  Copyright 1991 Bell
Communications Research, Inc. (Bellcore)


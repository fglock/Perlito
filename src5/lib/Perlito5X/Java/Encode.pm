package Encode;
use strict;

use Exporter qw(import);
our @EXPORT    = qw(encode_utf8 decode_utf8);
our @EXPORT_OK = qw(encode_utf8 decode_utf8 encode decode);

sub encode_utf8 ($) {
    return encode('UTF-8', $_[0]);
}

sub encode ($$;$) {
    return encode('UTF-8', $_[1], $_[2]) if $code eq 'utf8';
    eval {
        if (@_) {
        Java::inline '
            String charset = List__.shift().toString();
            String s = List__.shift().toString();
            byte[] bytes = s.getBytes(charset);
            StringBuilder sb = new StringBuilder();
            for (byte b : bytes) {
                int i = b < 0 ? 256 + b : b;
                sb.append((char)i);
            }
            return new PlString(sb.toString())
        ';
        }
        return undef;
    }
    or die $@;
}

sub decode_utf8 ($;$) {
    return decode('UTF-8', $_[0], $_[1]);
}

sub decode ($$;$) {
    return decode('UTF-8', $_[1], $_[2]) if $code eq 'utf8';
    eval {
        if (@_) {
        Java::inline '
            String charset = List__.shift().toString();
            String s = List__.shift().toString();
            char[] chars = s.toCharArray();
            byte[] bytes = new byte[chars.length];
            for (int i = 0; i < bytes.length; i++){
                bytes[i] = (byte)(chars[i]);
            }
            return new PlString(new String(bytes, charset))
        ';
        }
        return undef;
    }
    or die $@;
}

1;


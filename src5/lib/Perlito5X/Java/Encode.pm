package Encode;
use strict;

use Exporter qw(import);
our @EXPORT    = qw(encode_utf8 decode_utf8);
our @EXPORT_OK = qw(encode_utf8 decode_utf8 encode decode);

sub encode ($$;$) {
    my ($code) = @_;
    encode_utf8($_[1]) if $code eq 'utf8';
    die "not implemented: $code";
}

sub decode ($$;$) {
    my ($code) = @_;
    decode_utf8($_[1]) if $code eq 'utf8';
    die "not implemented: $code";
}

sub encode_utf8 ($) {
    eval {
        if (@_) {
        Java::inline '
            String s = List__.shift().toString();
            byte[] bytes = s.getBytes("UTF-8");
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
    eval {
        if (@_) {
        Java::inline '
            String s = List__.shift().toString();
            char[] chars = s.toCharArray();
            byte[] bytes = new byte[chars.length];
            for (int i = 0; i < bytes.length; i++){
                bytes[i] = (byte)(chars[i]);
            }
            return new PlString(new String(bytes, "UTF-8"))
        ';
        }
        return undef;
    }
    or die $@;
}

1;


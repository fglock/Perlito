package Encode;
use strict;

use Exporter qw(import);
our @EXPORT    = qw(encode_utf8 decode_utf8);
our @EXPORT_OK = qw(encode_utf8 decode_utf8);

sub encode_utf8 {
    eval {
        if (@_) {
        Java::inline '
            String s = List__.shift().toString();
            byte[] bytes = s.getBytes("UTF-8");
            StringBuilder sb = new StringBuilder();
            for (byte b : bytes) {
                sb.append((char)b);
            }
            return new PlString(sb.toString())
        ';
        }
        return undef;
    }
    or die $@;
}

sub decode_utf8 {
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


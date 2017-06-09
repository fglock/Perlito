package JSON;
use strict;

package Java::Json       { import => "javax.json.Json" }
package Java::JsonParser { import => "javax.json.stream.JsonParser" }
package Java::JsonParser::Event { import => "javax.json.stream.JsonParser.Event" }
package String {};
package StringReader     { import => "java.io.StringReader" }

sub parse {
    my $str = shift;

    my $result;

    my Java::JsonParser $parser = Java::Json->createParser(StringReader->new($str->toString()));
    while ($parser->hasNext()) {
        my Java::JsonParser::Event $event = $parser->next();
        my $ev = $event;
        say "event $ev";
        #if ($event == Java::JsonParser::START_ARRAY) {
        #    say "START_ARRAY";
        #}
    }

    return $result;
}

1;

__END__

# while (parser.hasNext()) {
#    JsonParser.Event event = parser.next();
#    switch(event) {
#       case START_ARRAY:
#       case END_ARRAY:
#       case START_OBJECT:
#       case END_OBJECT:
#       case VALUE_FALSE:
#       case VALUE_NULL:
#       case VALUE_TRUE:
#          System.out.println(event.toString());
#          break;
#       case KEY_NAME:
#          System.out.print(event.toString() + " " +
#                           parser.getString() + " - ");
#          break;
#       case VALUE_STRING:
#       case VALUE_NUMBER:
#          System.out.println(event.toString() + " " +
#                             parser.getString());
#          break;
#    }
# }

__END__


sub md5_hex {
    eval {
        my String $s = shift->toString();
        my $result = Java::BigInteger->new(1, Java::MessageDigest->getInstance("MD5")->digest($s->getBytes("UTF-8")))->toString(16);
        return $result;
    }
    or die $@;
}


import javax.json.Json;
import javax.json.stream.JsonParser;

use Exporter qw(import);
our @EXPORT    = qw(encode_utf8 decode_utf8);
our @EXPORT_OK = qw(encode_utf8 decode_utf8 encode decode);

sub encode_utf8 ($) {
    return encode('UTF-8', $_[0]);
}

sub encode ($$;$) {
    return encode('UTF-8', $_[1], $_[2]) if $_[0] eq 'utf8';
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
    return decode('UTF-8', $_[1], $_[2]) if $_[0] eq 'utf8';
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

__END__


=head1 COPYRIGHT

The original Encode module is

Copyright 2002-2014 Dan Kogai I<< <dankogai@cpan.org> >>.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut


...
JsonParser parser = Json.createParser(new StringReader(jsonData));
while (parser.hasNext()) {
   JsonParser.Event event = parser.next();
   switch(event) {
      case START_ARRAY:
      case END_ARRAY:
      case START_OBJECT:
      case END_OBJECT:
      case VALUE_FALSE:
      case VALUE_NULL:
      case VALUE_TRUE:
         System.out.println(event.toString());
         break;
      case KEY_NAME:
         System.out.print(event.toString() + " " +
                          parser.getString() + " - ");
         break;
      case VALUE_STRING:
      case VALUE_NUMBER:
         System.out.println(event.toString() + " " +
                            parser.getString());
         break;
   }
}

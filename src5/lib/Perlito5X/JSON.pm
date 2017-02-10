package JSON;
use Perlito5::JSON;
# use strict;

sub import {
    my $pkg     = shift;
    my $callpkg = caller(0);
    *{ $callpkg . "::encode_json" } = \&encode_json;
    *{ $callpkg . "::decode_json" } = \&decode_json;
    return;
}

sub encode_json {
    Perlito5::JSON::ast_dumper($_[0]);
}

sub _string_loop {
    my $s = "";
    while (1) {
        if ($_[0] =~ /\G"/gc) {
            # end-string
            return $s;
        }
        elsif ($_[0] =~ /\G\\([trnfbu\\\/])/gc) {
            # escape
            if ($1 eq 't') { $s .= "\t" }
            elsif ($1 eq 'r') { $s .= "\r" }
            elsif ($1 eq 'n') { $s .= "\n" }
            elsif ($1 eq 'f') { $s .= "\f" }
            elsif ($1 eq 'b') { $s .= "\b" }
            elsif ($1 eq '\\') { $s .= "\\" }
            elsif ($1 eq '/') { $s .= "/" }
            elsif ($1 eq 'u') {
                $_[0] =~ /\G([0-9a-fA-F]{4})/gc
                  or die "unexpected end of string while parsing JSON string, at character offset " . pos($_[0]);
                die "TODO \\u0000" 
            }
        }
        else {
            die "unexpected end of string while parsing JSON string, at character offset " . pos($_[0]);
        }
        if ($_[0] =~ /\G([^"\\]+)/gc) {
            $s .= $1;
        }
    }
} 

sub decode_json {
    pos($_[0]) = 0;
    &_decode_json;
}
sub _decode_json {
    $_[0] =~ /\G[ \t\r\n]+/gc;  # skip spaces
    if ($_[0] =~ /\G\[/gc) {
        # array
        my @r;
        $_[0] =~ /\G[ \t\r\n]+/gc;  # skip spaces
        if ($_[0] =~ /\G\]/gc) {
            # end-array
            return \@r;
        }
        while (1) {
            push @r, &_decode_json;
            $_[0] =~ /\G[ \t\r\n]+/gc;  # skip spaces
            if ($_[0] =~ /\G\]/gc) {
                # end-array
                return \@r;
            }
            elsif ($_[0] =~ /\G\,/gc) {
                # comma
            }
            else {
                die ", or ] expected while parsing array, at character offset " . pos($_[0]);
            }
        }
    }
    elsif ($_[0] =~ /\G\{/gc) {
        # object
        $_[0] =~ /\G[ \t\r\n]+/gc;  # skip spaces
        my %r;
        if ($_[0] =~ /\G\}/gc) {
            # end-object
            return \%r;
        }
        while (1) {
            if ($_[0] !~ /\G"([^"\\]*)/gc) {
                # not string
                die "unexpected end of string while parsing JSON string, at character offset " . pos($_[0]);
            }
            # string
            my $index = $1 . &_string_loop;

            # skip spaces, colon
            if ($_[0] !~ /\G[ \t\r\n]*:/gc) {
                # not colon
                die "unexpected end of string while parsing JSON string, at character offset " . pos($_[0]);
            }
            $r{$index} = &_decode_json;
            $_[0] =~ /\G[ \t\r\n]+/gc;  # skip spaces
            if ($_[0] =~ /\G\}/gc) {
                # end-object
                return \%r;
            }
            elsif ($_[0] =~ /\G\,[ \t\r\n]*/gc) {
                # comma + spaces
            }
            else {
                die "unexpected end of string while parsing JSON string, at character offset " . pos($_[0]);
            }
        }
    }
    elsif ($_[0] =~ /\G"([^"\\]*)/gc) {
        # string
        return $1 . &_string_loop;
    }
    elsif ($_[0] =~ /\G(-?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?)/gc) {
        # number
        return 0+$1;
    }
    elsif ($_[0] =~ /\Gfalse/gc) {
        # false
        return 0;   # TODO
    }
    elsif ($_[0] =~ /\Gtrue/gc) {
        # true
        return 1;   # TODO
    }
    elsif ($_[0] =~ /\Gnull/gc) {
        # null
        return undef;
    }
    else {
        die "malformed JSON string, neither tag, array, object, number, string or atom, at character offset " . pos($_[0]);
    }
}

1;

__END__


     use JSON; # imports encode_json, decode_json, to_json and from_json.
 
     # simple and fast interfaces (expect/generate UTF-8)
 
     $utf8_encoded_json_text = encode_json $perl_hash_or_arrayref;
     $perl_hash_or_arrayref  = decode_json $utf8_encoded_json_text;


=head1 SEE ALSO

Most of the document is copied and modified from JSON::XS doc.

L<JSON::XS>, L<JSON::PP>

C<RFC4627>(L<http://www.ietf.org/rfc/rfc4627.txt>)

=head1 AUTHOR

The original JSON module is

Makamaka Hannyaharamitu, E<lt>makamaka[at]cpan.orgE<gt>

JSON::XS was written by  Marc Lehmann <schmorp[at]schmorp.de>

The release of this new version owes to the courtesy of Marc Lehmann.


=head1 COPYRIGHT AND LICENSE

The original JSON module is

Copyright 2005-2013 by Makamaka Hannyaharamitu

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut


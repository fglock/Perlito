package JSON;
use Perlito5::JSON;

sub import {
    my $pkg     = shift;
    my $callpkg = caller(0);
    *{ $callpkg . "::encode_json" } = \&encode_json;
    return;
}

sub encode_json {
    Perlito5::JSON::ast_dumper($_[0]);
}

sub decode_json {
    die "TODO";
    while ($_[0] =~ /\G /g) {};  # skip spaces
    if ($_[0] =~ /\G\[/g) {
        # array
    }
    elsif ($_[0] =~ /\G{/g) {
        # hash
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


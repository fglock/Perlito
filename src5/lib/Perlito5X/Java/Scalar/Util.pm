package Scalar::Util;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(
    blessed refaddr reftype
    weaken unweaken isweak
    looks_like_number
);

sub refaddr {
    Java::inline("List__.aget(0).refaddr()");
}

sub reftype {
    Java::inline("List__.aget(0).reftype()");
}

sub blessed {
    Java::inline("List__.aget(0).blessed()");
}

sub weaken {
    # not implemented, all references are weak already
}

sub unweaken {
    # not implemented, can't make references "unweak"
}

sub isweak {
    # all references are weak already
    1;
}

sub looks_like_number {
    # TODO - '123A' => false
       (0 + $_[0])
    || ($_[0] eq '0')
    || ($_[0] eq '0E0')
}

1;

__END__

Test:

perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e 'use Scalar::Util "refaddr"; print refaddr(\1);'  > Main.java
javac Main.java
java Main

=head1 COPYRIGHT

The original Scalar::Util module is

Copyright (c) 1997-2007 Graham Barr <gbarr@pobox.com>. All rights reserved.
This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

Additionally L</weaken> and L</isweak> which are

Copyright (c) 1999 Tuomas J. Lukka <lukka@iki.fi>. All rights reserved.
This program is free software; you can redistribute it and/or modify it
under the same terms as perl itself.

Copyright (C) 2004, 2008  Matthijs van Duin.  All rights reserved.
Copyright (C) 2014 cPanel Inc.  All rights reserved.
This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut


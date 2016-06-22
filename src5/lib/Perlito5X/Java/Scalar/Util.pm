package Scalar::Util;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(
  blessed refaddr reftype
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

1;

__END__

Test:

perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e 'use Scalar::Util "refaddr"; print refaddr(\1);'  > Main.java
javac Main.java
java Main



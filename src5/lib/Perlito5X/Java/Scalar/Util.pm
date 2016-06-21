package Scalar::Util;
use strict;

sub refaddr {
    Java::inline("List__.aget(0).refaddr()");
}

1;

__END__

Test:

perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e 'use Scalar::Util "refaddr"; print refaddr(\1);'  > Main.java
javac Main.java
java Main



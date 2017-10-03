
print "1..2\n";

package Test;
use strict;

my $t = 123;

$t = time;
print "not " if $t == 123;
print "ok 1  # $t CORE::GLOBAL is not active yet\n";

BEGIN { *CORE::GLOBAL::time = sub { 456 } }

$t = time;
print "not " if $t != 456;
print "ok 2  # $t CORE::GLOBAL is active\n";



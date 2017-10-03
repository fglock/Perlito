
print "1..2\n";

package Test;
use strict;
use subs "time";

my $t = 123;

$t = time;
print "not " if $t != 456;
print "ok 1  # $t override is already active\n";

BEGIN { *time = sub { 456 } }

$t = time;
print "not " if $t != 456;
print "ok 2  # $t override is active\n";



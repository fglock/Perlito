
print "1..2\n";

package Test;
use strict;
use subs "time";

my $t = 123;

$t = time;
print "not " if $t != 789;
print "ok 1  # $t CORE::GLOBAL is not active\n";

BEGIN { *CORE::GLOBAL::time = sub { 456 } }
BEGIN { *time = sub { 789 } }

$t = time;
print "not " if $t != 789;
print "ok 2  # $t CORE::GLOBAL is still not active\n";



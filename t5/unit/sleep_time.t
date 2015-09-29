
use strict;

print "1..2\n";
my $t0 = time;
print "not " if !$t0;
print "ok 1 - time() returns something # $t0\n";
sleep 2;
my $t1 = time - $t0;
print "not " if $t1 < 1 || $t1 > 3;
print "ok 2 - time() changed after sleep # $t1\n";


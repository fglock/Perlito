
use strict;
use feature 'say';

say "1..5";

my $v = 0;

$v |= 2;
print "not " if $v != 2;
say "ok 1 - set # $v";

$v |= 8;
print "not " if $v != 10;
say "ok 2 - set # $v";

$v &= ~8;
print "not " if $v != 2;
say "ok 3 - unset # $v";

$v &= ~2;
print "not " if $v != 0;
say "ok 4 - unset # $v";

$v &= ~0;
print "not " if $v != 0;
say "ok 5 - unset # $v / " . ~0;


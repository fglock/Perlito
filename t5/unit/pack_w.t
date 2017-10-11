use feature 'say';
use strict;

say "1..4";

my $s;

$s = pack( "WWWW", 65, 66, 67, 68 );
print "not " if $s ne "ABCD";
say "ok 1";

$s = pack( "W4", 65, 66, 67, 68 );
print "not " if $s ne "ABCD";
say "ok 2";

$s = pack( "W4", 65, 66, 67 );
print "not " if $s ne "ABC\0";
say "ok 3";

$s = pack( "W0", 65, 66, 67 );
print "not " if $s ne "";
say "ok 4";



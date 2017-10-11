use feature 'say';
use strict;

say "1..6";

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

$s = pack( "W*", 65, 66, 67 );
print "not " if $s ne "ABC";
say "ok 5";

$s = pack("W4",0x24b6,0x24b7,0x24b8,0x24b9);
print "not " if $s ne "\x{24b6}\x{24b7}\x{24b8}\x{24b9}";
say "ok 6";


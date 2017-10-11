use feature 'say';
use strict;

say "1..2";

my $s;

$s = pack( "WWWW", 65, 66, 67, 68 );
print "not " if $s ne "ABCD";
say "ok 1";

$s = pack( "W4", 65, 66, 67, 68 );
print "not " if $s ne "ABCD";
say "ok 2";



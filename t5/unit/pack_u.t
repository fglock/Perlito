use feature 'say';
use strict;

say "1..7";

my $s;

$s = pack( "UUUU", 65, 66, 67, 68 );
print "not " if $s ne "ABCD";
say "ok 1";

$s = pack( "U4", 65, 66, 67, 68 );
print "not " if $s ne "ABCD";
say "ok 2";

$s = pack( "U4", 65, 66, 67 );
print "not " if $s ne "ABC\0";
say "ok 3";

$s = pack( "U0", 65, 66, 67 );
print "not " if $s ne "";
say "ok 4";

$s = pack("U4",0x24b6,0x24b7,0x24b8,0x24b9);
print "not " if $s ne "\x{24b6}\x{24b7}\x{24b8}\x{24b9}";
say "ok 5";

# "U0" mode

$s = pack("U0U4",0x24b6,0x24b7,0x24b8,0x24b9);
print "not " if $s ne "\x{24b6}\x{24b7}\x{24b8}\x{24b9}";
say "ok 6";

# character mode

$s = pack("C0U4",0x24b6,0x24b7,0x24b8,0x24b9);
print "not " if $s ne "\xe2\x92\xb6\xe2\x92\xb7\xe2\x92\xb8\xe2\x92\xb9";
say "ok 7";

use Data::Dumper;
print Dumper($s);

# $VAR1 = "\x{b6}\x{e2}\x{92}\x{b7}\x{e2}\x{92}\x{b8}\x{e2}\x{92}\x{b9}\x{0}";


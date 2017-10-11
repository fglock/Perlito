use feature 'say';
use strict;

say "1..7";

my @s;

sub is_array_like {
    return if scalar(@s) != scalar(@_);
    for (0 .. $#s) {
        return if $s[$_] != $_[$_];
    }
    return 1;
}

@s = unpack( "UUUU", "ABCD");
print "not " if !is_array_like (
    65, 66, 67, 68 );
say "ok 1";

@s = unpack( "U4", "ABCD");
print "not " if !is_array_like (
    65, 66, 67, 68 );
say "ok 2";

@s = unpack( "U4", "ABC\0");
print "not " if !is_array_like (
    65, 66, 67, 0 );
say "ok 3";

@s = unpack( "U0", "");
print "not " if !is_array_like ();
say "ok 4";

@s = unpack("U4", "\x{24b6}\x{24b7}\x{24b8}\x{24b9}");
print "not " if !is_array_like (
    0x24b6,0x24b7,0x24b8,0x24b9);
say "ok 5";

# "U0" mode

@s = unpack("U0U4", "\x{24b6}\x{24b7}\x{24b8}\x{24b9}");
print "not " if !is_array_like (
    0x24b6,0x24b7,0x24b8,0x24b9);
say "ok 6";

# character mode

@s = unpack("C0U4", "\xe2\x92\xb6\xe2\x92\xb7\xe2\x92\xb8\xe2\x92\xb9");
print "not " if !is_array_like (
    0x24b6,0x24b7,0x24b8,0x24b9);
say "ok 7";


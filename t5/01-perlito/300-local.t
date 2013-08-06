use v5;
use strict;
use feature 'say';

package X; # XXX javascript bug - we don't autovivify packages yet

package main;

say '1..7';

$X::v = 10;

my $vv = \$X::v;

if (1) {
    print "not " if $X::v != 10;
    say "ok 1";

    local $X::v;
    print "not " if defined $X::v;
    say "ok 2";

    print "not " if $$vv != 10;
    say "ok 3 # $$vv";

    $X::v = 15;
    print "not " if $X::v != 15;
    say "ok 4";

    print "not " if $$vv != 10;
    say "ok 5 # $$vv";

}

print "not " if $X::v != 10;
say "ok 6";

print "not " if $$vv != 10;
say "ok 7";


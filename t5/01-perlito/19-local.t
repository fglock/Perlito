use v5;
use strict;
use feature 'say';

package X; # XXX javascript bug - we don't autovivify packages yet

package main;

say '1..3';

$X::v = 10;

if (1) {
    print "not " if $X::v != 10;
    say "ok 1";
    local $X::v;
    $X::v = 15;
    print "not " if $X::v != 15;
    say "ok 2";
}

print "not " if $X::v != 10;
say "ok 3";


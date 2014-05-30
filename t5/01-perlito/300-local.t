use v5;
use strict;
use feature 'say';

package X; # XXX javascript bug - we don't autovivify packages yet

package main;

say '1..10';

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

# local index

my @x = ( 3, 6, 8, 4, 7 );

{
    local $x[3];

    print "not " if defined $x[3];
    say "ok 8";

    $x[3] = 9;
    my $res = "@x";
    print "not " if $res ne "3 6 8 9 7";
    say "ok 9 # $res";
}

my $res = "@x";
print "not " if $res ne "3 6 8 4 7";
say "ok 10 # $res"


use v5;
use strict;
use feature 'say';

say '1..10';

my $x = "abcd";
if (substr($x,1,1) ne "b") {
    print 'not '
};
say 'ok 1 - substr ', substr($x,1,1);

if (index($x,"c") ne 2) {
    print 'not '
};
say 'ok 2 - index ', index($x,"c");

if (substr($x,3,1) ne "d") {
    print 'not '
}
say 'ok 3 - substr ', substr($x,3,1);

print 'not ' if !defined "abc";
say 'ok 4 - defined str';

my $s = "o";
$s .= "k 5 - concat";
say $s;

$s = "The black cat climbed the green tree";
my $color  = substr $s, 4, 5;      # black
my $middle = substr $s, 4, -11;    # black cat climbed the
my $end    = substr $s, 14;        # climbed the green tree
my $tail   = substr $s, -4;        # tree
my $z      = substr $s, -4, 2;     # tr

print 'not ' if $color ne 'black';
say 'ok 6';

print 'not ' if $middle ne 'black cat climbed the';
say "ok 7  # $middle";

print 'not ' if $end ne 'climbed the green tree';
say 'ok 8';

print 'not ' if $tail ne 'tree';
say 'ok 9';

print 'not ' if $z ne 'tr';
say 'ok 10';


use feature 'say';
use strict;
use warnings;

say '1..2';

my $str = "Hello";
my $x = chop $str;

if ($x ne "o" || $str ne "Hell") {
    print 'not ';
}
say 'ok 1 - wanted o and Hell, got ' . $x . ' and ' . $str;

my @list = ("This", "is", "a", "list");
$x = chop @list;
if ($x ne "t") {
    print 'not ';
}
say 'ok 2 - wanted t, got ', $x;


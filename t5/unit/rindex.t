use feature 'say';
use strict;
use warnings;

say '1..4';

my $x = rindex('hello', 'e');
if ($x != 1) {
    print 'not ';
}
say 'ok 1 - wanted 1, got ', $x;

$x = rindex('hello', 'e', 0);
if ($x != -1) {
    print 'not ';
}
say 'ok 2 - wanted -1, got ', $x;

$x = rindex('hello', 'e', 2);
if ($x != 1) {
    print 'not ';
}
say 'ok 3 - wanted 1, got ', $x;

$x = rindex('hello', 'llo');
if ($x != 2) {
    print 'not ';
}
say 'ok 4 - wanted 2, got ', $x;


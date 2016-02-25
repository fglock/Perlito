use feature 'say';
use strict;
use warnings;

say '1..5';

my $x = hex "a";
if ($x != 10) {
    print 'not ';
}
say 'ok 1 - wanted 10, got ', $x;

$x = hex "A";
if ($x != 10) {
    print 'not ';
}
say 'ok 2 - wanted 10, got ', $x;

$x = hex "0xa";
if ($x != 10) {
    print 'not ';
}
say 'ok 3 - wanted 10, got ', $x;

$x = hex "0XA";
if ($x != 10) {
    print 'not ';
}
say 'ok 4 - wanted 10, got ', $x;

$x = hex "hello";
if ($x != 0) {
    print 'not ';
}
say 'ok 5 - wanted 0, got ', $x;

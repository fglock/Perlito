use feature 'say';
use strict;
use warnings;

say '1..4';

my $x = oct "8";
if ($x != 10) {
    print 'not ';
}
say 'ok 1 - wanted 10, got ', $x;

$x = oct "0xa";
if ($x != 10) {
    print 'not ';
}
say 'ok 2 - wanted 10, got ', $x;

$x = oct "0XA";
if ($x != 10) {
    print 'not ';
}
say 'ok 3 - wanted 10, got ', $x;

$x = oct "hello";
if ($x != 0) {
    print 'not ';
}
say 'ok 4 - wanted 0, got ', $x;

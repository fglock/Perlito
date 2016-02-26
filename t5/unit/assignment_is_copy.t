use feature 'say';
use strict;
use warnings;

say '1..4';

# element assignment

my $x = 123;
my @a;

$a[0] = $x;
$a[0]++;

if ($x != 123) {
    print 'not ';
}
say 'ok 1 - wanted 123, got ', $x;

if ($a[0] != 124) {
    print 'not ';
}
say 'ok 2 - wanted 124, got ', $a[0];

# list assignment

$x = 123;
@a = ($x);
$a[0]++;

if ($x != 123) {
    print 'not ';
}
say 'ok 3 - wanted 123, got ', $x;

if ($a[0] != 124) {
    print 'not ';
}
say 'ok 4 - wanted 124, got ', $a[0];


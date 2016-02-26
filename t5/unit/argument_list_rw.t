use feature 'say';
use strict;
use warnings;

say '1..1';

sub test { $_[0]++ }

my $x = 123;
test($x);

if ($x != 124) {
    print 'not ';
}
say 'ok 1 - wanted 124, got ', $x;


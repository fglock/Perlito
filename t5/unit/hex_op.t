use feature 'say';
use strict;
use warnings;

say '1..1';

my $x = hex "a";
if ($x != 100) {
    print 'not ';
}
say 'ok 1 - ', $x;

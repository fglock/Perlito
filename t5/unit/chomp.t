use feature 'say';
use strict;
use warnings;

say '1..2';

my $str = "Hello\n";
my $x = chomp $str;

if ($x != 1 || $str ne "Hello") {
    print 'not ';
}
say 'ok 1 - wanted 1, Hello got ', $x, ', ', $str;
$/ = "lo";
$x = chomp $str;
if ($x != 2 || $str ne "Hel") {
    print 'not ';
}
say 'ok 2 - wanted 2, Hel , got ', $x, ', ', $str;


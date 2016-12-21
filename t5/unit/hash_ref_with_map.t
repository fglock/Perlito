#!./perl -w

use feature 'say';
say( "1..2" );

my $foo = [5,200];
my $x = { (map { $_, ($_*2) } @$foo)};

print "not " if ($x->{5} != 10);
say ("ok 1 - map+hashref: x->5");

print "not " if ($x->{200} != 400);
say ("ok 2 - map+hashref: x->200");


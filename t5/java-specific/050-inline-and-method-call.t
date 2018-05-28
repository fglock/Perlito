
use v5;
use feature 'say';
use strict;

say "1..1";

my $x;

eval {
    $x = Java::inline q{ Class.forName("java.lang.Math") }
};

my $pi = $x->PI;
if ( substr($pi, 0, 4) ne "3.14") {
    print "not ";
}
say "ok 1 # $pi";

say "# done";


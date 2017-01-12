use v5;
use strict;
use feature 'say';

say '1..8';
my %a;
say 'ok 1 - create hash';
$a{abc}{def} = 3;
say 'ok 2 - set element';
if ($a{abc}{def} != 3) {
    print 'not '
}
say 'ok 3 - fetch element # ', $a{abc}{def};

my $b;
say 'ok 4 - create scalar';
$b->{abc}{def} = 3;
say 'ok 5 - set element';
if ($b->{abc}{def} != 3) {
    print 'not '
}
say 'ok 6 - fetch element # ', $b->{abc}{def};


sub dont_modify { $_[0] }

dont_modify( $b->{x} );
if (exists $b->{x}) {
    print 'not ';
}
say "ok 7 - don't vivify";


sub modify { $_[0] = 1 }

modify( $b->{x} );
if (! $b->{x}) {
    print 'not ';
}
say 'ok 8 - vivify through $_[0] aliasing';


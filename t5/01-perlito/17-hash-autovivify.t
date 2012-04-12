use v5;
use strict;
use feature 'say';

    say '1..6';
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

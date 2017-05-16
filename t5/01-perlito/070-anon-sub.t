use v5;
use strict;
use feature 'say';

package Main;
    say '1..4';
    my $a = sub { 3 };
    say 'ok 1 - create function';
    if ($a->() != 3) {
        print 'not '
    }
    say 'ok 2 - apply';
    $a = ( sub { 4 } )->();
    if ($a != 4) {
        print 'not '
    }
    say 'ok 3 - apply in line';
    $a = ( sub () { sub { 5 } } )->();
    if ($a->() != 5) {
        print 'not '
    }
    say 'ok 4 - return function';


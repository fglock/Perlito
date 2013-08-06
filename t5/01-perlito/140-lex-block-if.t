use v5;
use strict;
use feature 'say';

    say '1..2';
    my $a = 123;

    if (1) {
        my $a = do { 3 };
        if ($a != 3) {
            print 'not '
        }
        say 'ok 1 - if block';
    }

    if ($a != 123) {
        print 'not '
    }
    say 'ok 2 - if block # ', $a;

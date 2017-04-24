use v5;
use strict;
use feature 'say';

say '1..4';

my $x = 0;
if ( $x != 0 ) {
    print 'not ';
}
say 'ok 1 - ', $x;

my $x = 1;
if ( $x != 1 ) {
    print 'not ';
}
say 'ok 2 - ', $x;

{
    if ( $x != 1 ) {
        print 'not ';
    }
    say 'ok 3 - ', $x;

    my $x = 2;
    if ( $x != 2 ) {
        print 'not ';
    }
    say 'ok 4 - ', $x;
}

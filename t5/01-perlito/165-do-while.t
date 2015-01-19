use v5;
use strict;
use feature 'say';

# do {} while / do {} until
#  execute the loop once, before checking the condition

    say '1..5';
    my $a = 4;
    my $b = 0;
    do {
        $b = $b + 2;
        $a = $a - 1;
    } while ($a);
    if ($b == 8) {
        say "ok 1";
    }

    my @x = ( 2, 3, 4 );
    do {
        say "ok ", (shift @x); 
    } until @x;

    say "ok ", (shift @x)
        if @x; 

    @x = ( 4, 5 );
    say "ok ", (shift @x)
        while @x;

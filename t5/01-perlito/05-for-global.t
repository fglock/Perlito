use feature 'say';
use strict;

say '1..2';
my @a = ( 1, 2 );
for my $v (@a) {
    say 'ok ' . $v . ' - loop';
}


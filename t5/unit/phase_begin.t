use feature 'say';

say '1..1';

our $v;

BEGIN {
    $v = 123;
}

print 'not ' unless $v == 123;
say "ok 1 - BEGIN initialized an our-var # $v";


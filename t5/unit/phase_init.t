use feature 'say';

say '1..1';

our $v;

print 'not ' unless $v == 123;
say "ok 1 - INIT initialized an our-var # $v";

INIT {
    $v = 123;
}


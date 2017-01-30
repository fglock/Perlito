use feature 'say';

say '1..1';

our $v;

INIT {
    $v = 123;
}

print 'not ' unless $v == 123;
say "ok 1 - INIT initialized an our-var # $v";


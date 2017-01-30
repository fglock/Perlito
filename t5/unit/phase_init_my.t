use feature 'say';

say '1..1';

my $v;

print 'not ' unless $v == 123;
say "ok 1 - INIT initialized a my-var # $v";

INIT {
    $v = 123;
}


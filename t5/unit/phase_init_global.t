use feature 'say';

say '1..1';

print 'not ' unless $main::v == 123;
say "ok 1 - INIT initialized a global var # $main::v";

INIT {
    $main::v = 123;
}


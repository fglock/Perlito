use feature 'say';

say '1..1';

my $v;

BEGIN {
    $v = 123;
}

print 'not ' unless $v == 123;
say "ok 1 - BEGIN initialized a my-var # $v";


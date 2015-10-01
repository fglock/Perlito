use feature 'say';

say '1..2';

my $v = 1;

for (;;) {
    print "ok $v - infinite loop\n";
    last if $v++ == 2;
}


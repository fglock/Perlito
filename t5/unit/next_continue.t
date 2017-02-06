use feature 'say';

say '1..6';

my $n = 1;
for ( 1 .. 4 ) {
    if ($_ == 1 || $_ == 3) {
        say "# next";
        next;
    }
    say "ok 2 - for loop with \$_, next" if $_ == 2;
    say "ok 5 - for loop with \$_, next" if $_ == 4;
    $n++;
}
continue {
    say "ok $n - continue";
    $n++;
}


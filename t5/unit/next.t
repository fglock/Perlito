use feature 'say';

say '1..6';

for (1..4) {
    next if $_ == 1 || $_ == 3;
    say "ok ", ($_/2), " - for loop with \$_, next";
}

for (1..4) {
    {
        next if $_ == 1 || $_ == 3;
    }
    say "ok ", (2 + $_), " - for loop with \$_, next inside a block";
}


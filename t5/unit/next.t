use feature 'say';

say '1..8';

for ( 1 .. 4 ) {
    next if $_ == 1 || $_ == 3;
    say "ok ", ( $_ / 2 ), " - for loop with \$_, next";
}

for ( 1 .. 4 ) {
    {
        next if $_ == 1 || $_ == 3;
    }
    say "ok ", ( 2 + $_ ), " - for loop with \$_, next inside a block";
}

OUTER:
for my $i ( 1 .. 2 ) {
    for my $j ( 1 .. 4 ) {
        say "ok ", ( $i + 6 ), " - for loop, next LABEL";
        next OUTER if $j == 1 || $j == 3;
    }
}


use feature 'say';

say '1..8';

for ( 1 .. 4 ) {
    last if $_ == 1 || $_ == 3;
    say "ok ", ( $_ / 2 - 2 ), " - for loop with \$_, last";
}

for ( 1 .. 4 ) {
    {
        last if $_ == 1 || $_ == 3;
    }
    say "ok ", ( $_ ), " - for loop with \$_, last inside a block";
}

OUTER:
for my $i ( 1 .. 2 ) {
    for my $j ( 1 .. 4 ) {
        say "ok ", ( $i + 6 - 2 ), " - for loop, last LABEL";
        last OUTER if $j == 1 || $j == 3;
    }
}

my $count = 17;
while ($count < 20) {
    $count++;
    last if $count == 19;
    say "ok ", ( $count / 2 - 3 ), " - while loop, last";
}

{
    last;
    print "not ";
}
say "ok 7 - block last";

THIS: {
    last THIS;
    print "not ";
}
say "ok 8 - block last";


use feature 'say';

say '1..12';

for ( 1 .. 4 ) {
    eval { next if $_ == 1 || $_ == 3 };
    say "ok ", ( $_ / 2 ), " - for loop with \$_, next";
}

for ( 1 .. 4 ) {
    {
        eval { next if $_ == 1 || $_ == 3;}
    }
    say "ok ", ( 2 + $_ ), " - for loop with \$_, next inside a block";
}

OUTER:
for my $i ( 1 .. 2 ) {
    for my $j ( 1 .. 4 ) {
        say "ok ", ( $i + 6 ), " - for loop, next LABEL";
        eval { next OUTER if $j == 1 || $j == 3; };
    }
}

my $count = 17;
while ($count < 20) {
    $count++;
    eval { next if $count == 19; };
    say "ok ", ( $count / 2 ), " - while loop, next";
}

{
    eval { next; };
    print "not ";
}
say "ok 11 - block next";

THIS: {
    eval { next THIS; };
    print "not ";
}
say "ok 12 - block next";


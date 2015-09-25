use feature 'say';

say '1..12';

my $test = 1;
for ( 1 .. 4 ) {
    $_++ && redo if $_ == 1 || $_ == 3;
    say "ok ", ( $test++ ), " - for loop with \$_, redo";
}

$test = 5;
for ( 1 .. 4 ) {
    {
        $_++ && redo if $_ == 1 || $_ == 3;
    }
    say "ok ", ( $test++ ), " - for loop with \$_, redo inside a block";
}

$test = 9;
OUTER:
for my $i ( 1 .. 2 ) {
    last OUTER if $i > 2;
    for my $j ( 1 .. 4 ) {
        say "ok ", ( $test++ ), " - for loop, redo LABEL";
        $i++ && redo OUTER if $j == 1 || $j == 3;
    }
}

$test = 11;
my $count = 17;
while ($count < 20) {
    $count++;
    $count++ && redo if $count == 19;
    say "ok ", ( $test++ ), " - while loop, redo";
}



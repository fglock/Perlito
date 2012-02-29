use v6;

class Main {
    say '1..5';
    my $a = 4;
    my $b = 0;
    while $a {
        $b = $b + 2;
        $a = $a - 1;
    }
    if $b == 8 {
        say "ok 1";
    }

    my @x = ( 2, 3 );
    while @x {
        say "ok ", (shift @x); 
    }

    @x = ( 4, 5 );
    say "ok ", (shift @x)
        while @x;
}

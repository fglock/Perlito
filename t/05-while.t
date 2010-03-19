use v6;

class Main {
    say '1..1';
    my $a := 4;
    my $b := 0;
    while $a {
        $b := $b + 2;
        $a := $a - 1;
    }
    if $b == 8 {
        say "ok 1";
    }
}

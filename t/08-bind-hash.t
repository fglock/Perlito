use v6-alpha;

class Main {

    say '1..2';

    my $x := 1;
    my $y := 2;
    my $x1 := 0;
    my $y1 := 0;
    { :$x1, :$y1 } := { y1 => $y, x1 => $x };
    if $x1 != 1 {
        print 'not '
    };
    say 'ok ', $x1;
    if $y1 != 2 {
        print 'not '
    };
    say 'ok ', $y1;

}

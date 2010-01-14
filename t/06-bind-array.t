use v6;

class Main {

    say '1..2';

    my $x := 1;
    my $y := 2;
    my $x1 := 0;
    my $y1 := 0;
    [ $x1, $y1 ] := [ $x, $y ];
    if $x1 != 1 {
        print 'not '
    };
    say 'ok ', $x1;
    if $y1 != 2 {
        print 'not '
    };
    say 'ok ', $y1;

}

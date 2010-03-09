use v6;

class Main {

    say '1..3';

    my $x := 9;
    my $y := 10;
    my $x1 := 0;
    my $y1 := 0;
    my $z1 := 0;
    [ $x1, [ $y1, $z1 ] ] := [ $x, [$y, 11] ];
    if $x1 != 9 {
        print 'not '
    };
    say 'ok 1';
    if $y1 != 10 {
        print 'not '
    };
    say 'ok 2';
    if $z1 != 11 {
        print 'not '
    };
    say 'ok 3';

}

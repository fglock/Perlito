use v6;

class Main {
    say '1..4';
    my @a;
    say 'ok 1 - create array';
    @a[1] := 3;
    say 'ok 2 - set element';
    if @a[1] != 3 {
        print 'not '
    }
    say 'ok 3 - fetch element # ', @a[1];

    my @x := [ 3, 4 ]; 
    @a := [ 1, @x, 2 ]; 
    if @a[1] != 3 {
        print 'not '
    }
    say 'ok 4 - interpolate array # ', @a;
}

use v6;

class Obj {
    has $.x;
    has $.y;
}

class Main {

    say '1..2';

    my $x := 0;
    my $y := 0;
    my $obj := ::Obj( x => 1, y => 2 ); 
    ::Obj( :$x, :$y ) := $obj;
    if $x != 1 {
        print 'not '
    };
    say 'ok ', $x;
    if $y != 2 {
        print 'not '
    };
    say 'ok ', $y;

}

use v6-alpha;

class Other {

    method subr( $a, $b ) { $a + $b };
    method subr2( $a, $b, $c ) { $a + $b + $c };
}

class Main {
    
    say '1..2';

    my $other := ::Other();

    my $x := 0;
    $x := $other.subr( 1, 2 );
    if $x != 3 {
        print 'not '
    };
    say 'ok 1 - ', $x;



    $x := 0;
    $x := Other.subr2( 1, 2, 4 );
    if $x != 7 {
        print 'not '
    };
    say 'ok 2 - ', $x;

}

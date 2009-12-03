use v6-alpha;

class Main {
    
    say '1..2';

    method subr( $a, $b ) { $a + $b };

    my $x := 0;
    $x := Main.subr( 1, 2 );
    if $x != 3 {
        print 'not '
    };
    say 'ok 1 - ', $x;


    method subr2( $a, [ $b, $c ] ) { $a + $b + $c };

    $x := 0;
    $x := Main.subr2( 1, [ 2, 4 ] );
    if $x != 7 {
        print 'not '
    };
    say 'ok 2 - ', $x;

}

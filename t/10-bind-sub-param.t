use v6-alpha;

class Main {
    
    say '1..3';

    sub subr( $a, $b ) { $a + $b };

    my $x := 0;
    $x := subr( 1, 2 );
    if $x != 3 {
        print 'not '
    };
    say 'ok 1 - ', $x;

    sub subr3( @x ) { @x[0] + @x[1] };

    $x := 0;
    $x := subr3( [3, 4] );
    if $x != 7 {
        print 'not '
    };
    say 'ok 2 - ', $x;

    # "Main" namespace

    $x := 0;
    $x := Main::subr( 1, 2 );
    if $x != 3 {
        print 'not '
    };
    say 'ok 3 - ', $x;

}

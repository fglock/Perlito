use v6;

say '1..4';

sub subr( $a, $b ) { $a + $b };

class Mod2 {
    sub subr( $a, $b ) { $a + $b + 1 }
}

my $x = 0;
$x = subr( 1, 2 );
if $x != 3 {
    print 'not '
};
say 'ok 1 - ', $x;

sub subr3( @x ) { @x[0] + @x[1] };

$x = 0;
$x = subr3( [3, 4] );
if $x != 7 {
    print 'not '
};
say 'ok 2 - ', $x;

# we are in the default "GLOBAL" namespace

$x = 0;
$x = GLOBAL::subr( 1, 2 );
if $x != 3 {
    print 'not '
}
say 'ok 3 - ', $x;

if Mod2::subr( 1, 2 ) != 4 {
    print 'not '
}
say 'ok 4 - ', Mod2::subr( 1, 2 );


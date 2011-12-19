package Main;
    
say '1..3';

sub subr {
    my $v1 = shift;
    my $v2 = shift;
    $v1 + $v2 
}

my $x = 0;
$x = subr( 1, 2 );
if $x != 3 {
    print 'not '
};
say 'ok 1 - ', $x;

sub subr3 { 
    my @x = @$_[0];
    $x[0] + $x[1] 
};

$x = 0;
$x = subr3( [3, 4] );
if $x != 7 {
    print 'not '
};
say 'ok 2 - ', $x;

# "Main" namespace

$x = 0;
$x = Main::subr( 1, 2 );
if $x != 3 {
    print 'not '
};
say 'ok 3 - ', $x;


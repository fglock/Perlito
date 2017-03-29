
print "1..2\n";

BEGIN {
    for my $v ( 3 .. 5 ) {
        *{"v1_$v"} = sub { $v };
        *{"v2_$v"} = sub { $v };
        *{"incr_$v"} = sub { $v++ };
    }
}

my $x;

$x = v1_3 ();
if ( $x != 3 ) {
    print "not ";
}
print "ok 1 - get shared variable # $x\n";

incr_3();

$x = v1_3 ();
if ( $x != 4 ) {
    print "not ";
}
print "ok 2 - increment shared variable # $x\n";



print "1..2\n";

BEGIN {
    for my $v ( 3 .. 5 ) {
        *{"v$v"} = sub { $v }
    }
}

my $x;

$x = v3 ();
if ( $x != 3 ) {
    print "not ";
}
print "ok 1  - create named subroutine in BEGIN # $x\n";

$x = v5 ();
if ( $x != 5 ) {
    print "not ";
}
print "ok 2  - create named subroutine in BEGIN # $x\n";


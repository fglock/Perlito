use strict;

print "1..2\n";

sub subr {
    my ($count) = @_;
    my $x;

    print "# at count [$count] value is [$x]\n";

    if ( $count == 1 ) {
        if ( !$x || $x ne "compile-time" ) {
            print "not ";
        }
    }
    else {
        if ($x) {
            print "not ";
        }
    }
    print "ok $count\n";

    BEGIN { $x = "compile-time"; }
}

subr(1);
subr(2);


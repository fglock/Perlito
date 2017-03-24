use strict;

print "1..6\n";

sub subr {
    my ($count, $series) = @_;
    my $x;

    print "# at count [$count] value is [$x]\n";

    if ( $count == 1 && $series == 0 ) {
        if ( !$x || $x ne "compile-time" ) {
            print "not ";
        }
    }
    else {
        if ($x) {
            print "not ";
        }
    }
    print "ok ", ($count + $series), "\n";

    if ( $count < 3 ) {

        subr( $count + 1, $series );

    }

    print "# done [$count]\n";

    BEGIN { $x = "compile-time"; }
}

subr(1, 0);
subr(1, 3);


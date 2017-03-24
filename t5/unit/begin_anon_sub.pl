use strict;

print "1..2\n";

my $subr = sub {
    my ($count) = @_;
    my $x;

    print "# at count [$count] value is [$x]\n";

    if ($x) {
        print "not ";
    }
    print "ok $count\n";

    BEGIN { $x = "compile-time"; }
};

$subr->(1);
$subr->(2);


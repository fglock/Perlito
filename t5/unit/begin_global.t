
print "1..5\n";

BEGIN {
    $_ = 123;
}

if ( $_ != 123 ) {
    print "not ";
}
print "ok 1  - init \$_ in BEGIN # $_\n";



BEGIN {
    $a = 123;
}

if ( $a != 123 ) {
    print "not ";
}
print "ok 2  - init \$a in BEGIN # $a\n";


BEGIN {
    $main::x = 123;
    @main::x = (456, 789);
    %main::x = (zz => 999);
}

if ( $main::x != 123 ) {
    print "not ";
}
print "ok 3  - init \$main::x in BEGIN # $main::x\n";


if ( "@main::x" ne "456 789" ) {
    print "not ";
}
print "ok 4  - init \@main::x in BEGIN # @main::x\n";


my $v = "@{[ %main::x ]}";
if ( $v ne "zz 999" ) {
    print "not ";
}
print "ok 5  - init \%main::x in BEGIN # $v\n";


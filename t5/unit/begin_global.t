
print "1..3\n";


BEGIN {
    $main::x = 123;
    @main::x = (456, 789);
    %main::x = (zz => 999);
}

if ( $main::x != 123 ) {
    print "not ";
}
print "ok 1  - init \$main::x in BEGIN # $main::x\n";


if ( "@main::x" ne "456 789" ) {
    print "not ";
}
print "ok 2  - init \@main::x in BEGIN # @main::x\n";


my $v = "@{[ %main::x ]}";
if ( $v ne "zz 999" ) {
    print "not ";
}
print "ok 3  - init \%main::x in BEGIN # $v\n";


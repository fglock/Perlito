
print "1..3\n";

my $x;
my @x;
my %x;

BEGIN {
    $x = 123;
    @x = (456, 789);
    %x = (zz => 999);
}

if ( $x != 123 ) {
    print "not ";
}
print "ok 1  - init \$x in BEGIN # $x\n";


if ( "@x" ne "456 789" ) {
    print "not ";
}
print "ok 2  - init \@x in BEGIN # @x\n";


my $v = "@{[ %x ]}";
if ( $v ne "zz 999" ) {
    print "not ";
}
print "ok 3  - init \%x in BEGIN # $v\n";


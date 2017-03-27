
print "1..3\n";

for my $z ( "1", "2", "3" ) {
    my $x;
    print "not " if $z eq "1" && $x ne "init";
    print "not " if $z ne "1" && $x eq "init";
    print "ok $z # $x\n";
    INIT { $x = "init" }
}


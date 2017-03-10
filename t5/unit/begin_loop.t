
print "1..3\n";

for my $z ( "3", "", "" ) {
    my $x;
    print "not " if $x ne $z;
    print "ok # $x\n";
    BEGIN { $x = 3 }
}


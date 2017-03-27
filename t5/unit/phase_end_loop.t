
print "1..1\n";

for my $z ( "a", "b", "c" ) {
    my $x = $z;

    END {
        print "not " if $x ne "a";
        print "ok # $x\n";
    }
}


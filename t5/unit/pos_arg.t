
print "1..2\n";

$x = "345678";

sub x { $_[0] =~ /\d/g }

x($x);
if ( pos($x) != 1 ) {
    print "not ";
}
print "ok 1  # pos() in an aliased subroutine argument ", pos($x), "\n";

x($x);
if ( pos($x) != 2 ) {
    print "not ";
}
print "ok 2  # ", pos($x), "\n";


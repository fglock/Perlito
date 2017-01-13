our $x;

print "1..8\n";

if ( defined $x ) {
    print "not ";
}
print "ok 1  # ", $x, "\n";

$x = 'a';
if ( $x ne 'a' ) {
    print "not ";
}
print "ok 2  # ", $x, "\n";

{
    if ( $x ne 'a' ) {
        print "not ";
    }
    print "ok 3 - inside block  # ", $x, "\n";

    local $x;
    if ( defined $x ) {
        print "not ";
    }
    print "ok 4  # ", $x, "\n";

    $x = "r";
    if ( $x ne 'r' ) {
        print "not ";
    }
    print "ok 5  # ", $x, "\n";

}
if ( $x ne 'a' ) {
    print "not ";
}
print "ok 6 - out of block retrieves previous value # ", $x, "\n";

{
    if ( $x ne 'a' ) {
        print "not ";
    }
    print "ok 7 - inside block  # ", $x, "\n";

    local $x = $x;
    if ( $x ne 'a' ) {
        print "not ";
    }
    print "ok 8 # ", $x, "\n";
}

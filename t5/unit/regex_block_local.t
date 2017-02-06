
    print "1..5\n";

    if ( defined $1 ) {
        print "not ";
    }
    print "ok 1  # ", $1, "\n";

    $_ = 'bar';
    $_ =~ m/\w(\w)/g;
    if ( $1 ne 'a' ) {
        print "not ";
    }
    print "ok 2  # ", $1, "\n";

    {
        if ( $1 ne 'a' ) {
            print "not ";
        }
        print "ok 3 - inside block  # ", $1, "\n";

        $_ =~ m/(\w)/g;
        if ( $1 ne 'r' ) {
            print "not ";
        }
        print "ok 4  # ", $1, "\n";

    }
    if ( $1 ne 'a' ) {
        print "not ";
    }
    print "ok 5 - out of block retrieves previous value # ", $1, "\n";


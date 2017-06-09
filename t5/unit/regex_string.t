
    print "1..2\n";

    $_ = 'bar';
    $_ =~ q{(\w)};
    if ( $1 ne 'b' ) {
        print "not ";
    }
    print "ok 1  # ", $1, "\n";

    $_ =~ ( q{\w} . q{(\w)} );
    if ( $1 ne 'a' ) {
        print "not ";
    }
    print "ok 2  # ", $1, "\n";


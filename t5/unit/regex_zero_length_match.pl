
    print "1..4\n";

    $_ = 'bar';
    $_ =~ m/\w??/g;
    if (pos != 0) {
        print "not "
    }
    print "ok 1  # ",pos(),"\n";

    $_ =~ m/\w??/g;
    if (pos != 1) {
        print "not "
    }
    print "ok 2  # ",pos(),"\n";

    $_ =~ m/\w??/g;
    if (pos != 1) {
        print "not "
    }
    print "ok 3  # ",pos(),"\n";

    $_ =~ m/\w??/g;
    if (pos != 2) {
        print "not "
    }
    print "ok 4  # ",pos(),"\n";


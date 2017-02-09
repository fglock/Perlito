
    print "1..4\n";

    $_ = 'bar123';
    $_ =~ m/\w/g;
    if (pos != 1) {
        print "not "
    }
    print "ok 1  # ",pos(),"\n";

    $_ =~ m/\G\d/gc;
    if (pos() != 1) {
        print "not "
    }
    print "ok 2  # anchored \\G should not match ",pos(),"\n";

    $_ =~ m/\G\w/g;
    if (pos != 2) {
        print "not "
    }
    print "ok 3  # pos is not reset ",pos(),"\n";


    $_ =~ m/\G\w/g;
    if (pos != 3) {
        print "not "
    }
    print "ok 4  # continue ",pos(),"\n";


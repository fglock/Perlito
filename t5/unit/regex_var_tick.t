
    print "1..4\n";

    $_ = 'bar';
    $_ =~ m/a/g;
    if ($` ne 'b') {
        print "not "
    }
    print "ok 1  # ",$`,"\n";

    if ($' ne 'r') {
        print "not "
    }
    print "ok 2  # ",$',"\n";


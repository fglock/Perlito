
    print "1..4\n";

    $_ = 'bar';
    $_ =~ m/\w??/g;
    if ($& ne '') {
        print "not "
    }
    print "ok 1  # ",$&,"\n";

    $_ =~ m/\w/g;
    if ($& ne 'b') {
        print "not "
    }
    print "ok 2  # ",$&,"\n";

    $_ =~ m/x/g;
    if ($& ne 'b') {
        print "not "
    }
    print "ok 3  # ",$&,"\n";

    $_ =~ m/\w\w/g;
    if ($& ne 'ba') {
        print "not "
    }
    print "ok 4  # ",$&,"\n";


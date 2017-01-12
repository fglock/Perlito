
    print "1..6\n";

    if (defined $1) {
        print "not "
    }
    print "ok 1  # ",$1,"\n";

    $_ = 'bar';
    $_ =~ m/\w??/g;
    if ($1 ne '') {
        print "not "
    }
    print "ok 2  # ",$1,"\n";
    if (defined $1) {
        print "not "
    }
    print "ok 3  # ",$1,"\n";

    $_ =~ m/(\w)/g;
    if ($1 ne 'b') {
        print "not "
    }
    print "ok 4  # ",$1,"\n";

    $_ =~ m/x/g;
    if ($1 ne 'b') {
        print "not "
    }
    print "ok 5  # ",$1,"\n";

    $_ =~ m/\w(\w)/g;
    if ($1 ne 'a') {
        print "not "
    }
    print "ok 6  # ",$1,"\n";


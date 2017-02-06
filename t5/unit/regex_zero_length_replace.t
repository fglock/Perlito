
    print "1..1\n";

    $_ = 'bar';
    s/\w??/<$&>/g;

    if ($_ ne '<><b><><a><><r><>') {
        print "not "
    }
    print "ok 1  # $_ \n";


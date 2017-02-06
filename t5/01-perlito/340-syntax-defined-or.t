
print "1..4\n";

sub shift_test {
    @_ = (5, 6);
    
    print "not " unless (shift // 2) == 5;
    print "ok 1 # shift and defined-or\n";
    
    #
    #   # syntax error
    #
    #   print "not " unless (shift //) == 5;
    #   print "ok 1 # shift and match\n";
    
    #
    #   # Warning: Use of "shift" without parentheses is ambiguous
    #   
    #   print "not " unless (shift / 2) == 3;
    #   print "ok 2 # shift and division\n";
}

shift_test;



sub testing {
    123;
} 

sub more_test {

    #
    #   # Number found where operator expected near "// 2"
    #   #    (Missing operator before  2?)
    #   # syntax near "// 2"
    #
    #   print "not " unless (testing // 2) == 5;
    #   print "ok 1 # sub and defined-or\n";

    print "not " unless (testing //);
    print "ok 2 # sub and match\n";

    #
    #   # Search pattern not terminated
    #
    #   print "not " unless (testing / 2) == 3;
    #   print "ok 2 # sub and division\n";
}

more_test;



sub testing2 () {
    120;
} 

sub more_test2 {
    
    # Number found where operator expected near "// 2"
    #    (Missing operator before  2?)
    # syntax near "// 2"
    
    print "not " unless (testing2 // 2) == 120;
    print "ok 3 # unary and defined-or\n";

    #
    #   # syntax error
    #
    #   print "not " unless (testing2 //);
    #   print "ok 1 # unary and match\n";
    
    print "not " unless (testing2 / 2) == 60;
    print "ok 4 # unary and division\n";
}

more_test2;


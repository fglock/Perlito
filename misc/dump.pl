BEGIN {
    for my $x ( 4 .. 6 ) {

        *{"mm$x"} = sub {
            if ( @_ && ref($_[0]) eq "Perlito5::dump" ) { return "do { my \$x = $x; sub { \$_[0] + \$x;  } }" }
            $_[0] + $x;
          };

    }
}

BEGIN {
    
    my $flag = bless {}, "Perlito5::dump";

    print "compile-time\n";

    my $y = \&mm4;
    print $y->($flag), "\n", $y->(456), " ", mm4(0), "\n";

    $y = \&mm5;
    print $y->($flag), "\n", $y->(456), "\n";

    $y = \&mm6;
    print $y->($flag), "\n", $y->(456), "\n";
}


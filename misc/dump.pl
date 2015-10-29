BEGIN {
    for my $x ( 4 .. 6 ) {

        *{"mm$x"} = sub {
            if ( $_[0] eq "dump" ) { return "do { my \$x = $x; sub { \$_[0] + \$x;  } }" }
            $_[0] + $x;
          };

    }
}

BEGIN {
    
    print "compile-time\n";

    my $y = \&mm4;
    print $y->("dump"), "\n", $y->(456), " ", mm4(0), "\n";

    $y = \&mm5;
    print $y->("dump"), "\n", $y->(456), "\n";

    $y = \&mm6;
    print $y->("dump"), "\n", $y->(456), "\n";
}


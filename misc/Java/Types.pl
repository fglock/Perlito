    package String { }
    package Integer { }
    package Boolean { }
    package Double { }
    package Byte { }
    package Short { }
    package Float { }
    package Long { }

    my String $x  = "abc";
    my Integer $y = 123;
    my $v         = Boolean->TRUE;
    print "$v\n";
    {
        my Byte $b = 100;
        my $pb = $b;
        $b = $pb;
    }
    {
        my Short $b = 100;
        my $pb = $b;
        $b = $pb;
    }
    {
        my $v = 100;
        my Float $b = $v;
        my $pb = $b;
        $b = $pb;
    }
    {
        my $v = 100;
        my Long  $b = $v;
        my $pb = $b;
        $b = $pb;
    }


    package String { }
    package Integer { }
    package Boolean { }
    package Double { }
    package Byte { }
    package Short { }

    my String $x  = "abc";
    my Integer $y = 123;
    my $v         = Boolean->TRUE;
    print "$v\n";
    {
        my Byte $b = 100;
        my $pb = $b;
        $b = $pb->to_byte();

      # TODO: automatic coercion from Perl to Java
      # $b = $pb;
    }
    {
        my Short $b = 100;
        my $pb = $b;
        $b = $pb->to_short();

      # TODO: automatic coercion from Perl to Java
      # $b = $pb;
    }


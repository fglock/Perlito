use feature 'say';

print "1..12\n";

# TODO:
#
#    $ perl -e ' { package X; sub print { CORE::print(">$_[1]<\n") } } my $x = bless {}, "X"; print $x "xxx" '
#    Not a GLOB reference at -e line 1.
#
#    $ perl -e ' { package X; sub printx { CORE::print(">$_[1]<\n") } } my $x = bless {}, "X"; printx $x "xxx" '
#    >xxx<
#
#    $ perl -MO=Deparse -e ' print X:: "xxx" '
#    print X 'xxx';
#
#    $ perl perlito5.pl -MO=Deparse -e ' ::X::x::y '
#    join("", ::{'main::X::'} x main::y);
#
#    sub t { 123 . ($_[0] || 'undef') }
#    # Can't locate object method "t" via package "X" (perhaps you forgot to load "X"?)
#    $v = t X:: "y";
#    print ">$v<\n";


my $v;
my $r;
my $x;

{
    use strict; 
    $r = 3;
    sub A { $r = 4 }
    ::A;   
    print "not " if $r != 4;
    say "ok 1 - double-colon before # ::A $r ";    # 4

    $r = 3;
    main::A;   
    print "not " if $r != 4;
    say "ok 2 - double-colon before means 'main::A' # ::A $r ";    # 4
}

{
    use strict; 
    eval ' $x = B:: '; 
    $r = ">$x<";
    print "not " if $r ne ">B<";
    say "ok 3 - double-colon after - B:: $x";    # B
}

{
    no strict;
    $x = C;
    $r = ">$x<";
    print "not " if $r ne ">C<";
    say "ok 4 - no double-colon before or after # C $x ";    # C
}

{
    use strict;
    $r = eval   "
                $x = D;
                $v = '>' . $x . '<';
                1; "
            || 0;
    print 'not ' if $r;
    say 'ok 5 - Bareword "D" not allowed while "strict subs" in use';
}

{
    no strict;

    $x = ::E; 
    $r = ">$x<";
    print "not " if $r ne ">::E<";
    say "ok 6 - double-colon before - ::E $x";
}

{
    use strict;

    eval q[ $F'x = 9      ];
    eval q[ $r = ">$F'x<" ];
    print "not " if $r ne ">9<";
    say "ok 7 - tick instead of double-colon - \$F'x $r # TODO Parser bug";

    eval q[ $r = ">$F::x<" ];
    print "not " if $r ne ">9<";
    say "ok 8 - double-colon instead of tick - \$F::x $r # TODO Parser bug";
}

{
    no strict;

    eval q[ $x = ::G'a ]; 
    $r = ">$x<";
    print "not " if $r ne ">::G::a<";
    say "ok 9 - tick in constant - ::G'a $x # TODO Parser bug";
}

{
    package main;
    use strict; 
    use warnings;

    my $r;
    {
        package HH;
        sub H { $r = 4 }
    }

    $r = 3;
    ::HH->H();   
    print "not " if $r != 4;
    say "ok 10 - double-colon before # ::HH->H $r ";    # 4

    $r = 3;
    main::HH->H();   
    print "not " if $r != 4;
    say "ok 11 - double-colon before means 'main::H' # main::HH->H $r ";    # 4

    my $o = bless {}, 'HH';

    # Bareword found where operator expected
    # $r = 3;
    # $o->::HH::H();   
    # print "not " if $r != 4;
    # say "ok 12 - double-colon before # o->::HH::H $r ";    # 4

    $r = 3;
    $o->main::HH::H();   
    print "not " if $r != 4;
    say "ok 12 - double-colon before means 'main::H' # o->main::HH::H $r ";    # 4

}



use feature 'say';

print "1..6\n";

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
    my $x = B::; 
    $r = ">$x<";
    print "not " if $r ne ">B<";
    say "ok 3 - double-colon after # B:: $x ";    # B
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
    my $r = eval   "
                $x = D;
                $v = '>' . $x . '<';
                1; "
            || 0;
    print 'not ' if $r;
    say 'ok 5 - Bareword "D" not allowed while "strict subs" in use';
}

{
    my $x = ::E; 
    $r = ">$x<";
    print "not " if $r ne ">::E<";
    say "ok 6 - double-colon before # ::E $x ";    # E
}


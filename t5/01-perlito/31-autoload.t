use feature 'say';
use strict;

say "1..5";

{

    package A;

    our $AUTOLOAD;

    sub AUTOLOAD {
        say "# A::AUTOLOAD @_";
        print "not " if $_[0] != 123;
        say "ok 1";
        say "# $AUTOLOAD";
        print "not " if $AUTOLOAD ne "A::a";
        say "ok 2";
        return wantarray ? ( 4, 5 ) : 456;
    }

    my $v = a(123);
    print "not " if $v != 456;
    say "ok 3";

}

{

    package C;

    our $AUTOLOAD;

    sub AUTOLOAD {
        return wantarray ? ( 6, 7 ) : 456;
    }

    my @x = a(123);
    print "not " if $x[0] != 6 || $x[1] != 7;
    say "ok 4";

    {
        no strict;
        my $v = XYZ;
        print "not " if $v == 456;
        say "ok 5 # bareword doesn't call AUTOLOAD";
    }
}


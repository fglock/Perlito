use feature 'say';
use strict;

say "1..3";

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
        return 456;
    }

    my $v = a(123);
        print "not " if $v != 456;
        say "ok 3";
}



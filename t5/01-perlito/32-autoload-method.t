use feature 'say';
use strict;

say "1..4";

{
    package A;

    our $AUTOLOAD;

    sub AUTOLOAD {
        say "# A::AUTOLOAD @_";
        print "not " if $_[0] ne 'A';
        say "ok 1";
        print "not " if $_[1] != 123;
        say "ok 2";
        say "# $AUTOLOAD";
        print "not " if $AUTOLOAD ne "A::a";
        say "ok 3";
        return 456;
    }

    my $v = A->a(123);
        print "not " if $v != 456;
        say "ok 4";
}



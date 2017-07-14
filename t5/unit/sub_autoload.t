use feature 'say';
use strict;

say "1..10";

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

{
    package D;
    our $AUTOLOAD;
    sub AUTOLOAD {
        print "not ";
        return wantarray ? ( 6, 7 ) : 456;
    }
    my $x = prototype("a");
    print "not " if $x;
    say "ok 6 - prototype() doesn't call AUTOLOAD # $x ";
}

{
    package E;
    our $AUTOLOAD;
    sub AUTOLOAD {
        print "not ";
        return wantarray ? ( 6, 7 ) : 456;
    }
    my $x = defined &{"a"};
    print "not " if $x;
    say "ok 7 - defined() doesn't call AUTOLOAD # $x ";
}

{
    package F;
    our $AUTOLOAD;
    sub AUTOLOAD {
        return wantarray ? ( 6, 7 ) : 456;
    }
    no strict 'refs';
    my $x = &{"a"};
    print "not " if !$x;
    say "ok 8 - code-deref calls AUTOLOAD # $x ";
}

{
    package G;
    our $AUTOLOAD;
    sub AUTOLOAD {
        print "not ";
        return wantarray ? ( 6, 7 ) : 456;
    }
    my $x = exists &{"a"};
    print "not " if $x;
    say "ok 9 - exists() doesn't call AUTOLOAD # $x ";
}

{
    package H;
    our $AUTOLOAD;
    my $f = 1;
    sub AUTOLOAD {
        $f = 2;
        print "# HERE\n";
        return wantarray ? ( 6, 7 ) : 456;
    }
    my $x = \&{"subr"};
    print "not " if $f != 1;
    say "ok 10 - ref doesn't call AUTOLOAD # $x ";
}



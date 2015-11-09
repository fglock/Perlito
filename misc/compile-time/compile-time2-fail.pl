use strict;
use warnings;

# Note - this script has 2 problems:
# - variable redeclarations,
# - sharing compile-time lexicals in inner closures
#
# Variable "$z" will not stay shared at x.pl line 8.
# Variable "$z" will not stay shared at x.pl line 9.
# "my" variable $z masks earlier declaration in same scope at x.pl line 10.
# Variable "$z" will not stay shared at x.pl line 11.
# Variable "$z" will not stay shared at x.pl line 12.
# Variable "$z" will not stay shared at x.pl line 14.
# Variable "$z" will not stay shared at x.pl line 15.
#

#--------------------------

    # original code
    my $v = 3;
    sub z0 {
        my $z = shift;
        sub z1 { eval ' print "in eval $z\\n" '; $z }
        BEGIN { $z = 5 }
        my $z = shift;      # redefine $z
        sub z2 { eval ' print "in eval $z\\n" '; $z }
        BEGIN { $z = 7 }
        for my $z (32..35) {
            sub z3 { eval ' print "in eval $z\\n" '; $z }
            BEGIN { $z = 9 }
            print "z3() ", z3(), "\n";
        }
    }
    print z1, "\n"; # 5
    print z2, "\n"; # 7
    print z3, "\n"; # 9
    z0(10, 15);
    print z1, "\n"; # 10
    print z2, "\n"; # 15
    print z3, "\n"; # 9
    z0(20, 25);
    print z1, "\n"; # still 10
    print z2, "\n"; # still 15
    print z3, "\n"; # 9
    print $v, "\n"; # 3
    my $count = 0;
    {
        my $z;
        BEGIN { $z = 40 }
        print "block $z\n";
        $z++ && redo
            if $count++ < 2;
    }

  
    # after compile-time env
    # 'my' variables in the compile-time scratchpad
    my @v__0 = (undef);  # $v is not initialized at compile-time
    my @z__0 = (5);  # BEGIN ... *side effect* on a captured lexical
    my @z__1 = (7);  # BEGIN ... *side effect* on a captured lexical
    my @z__2 = (9);  # BEGIN ... *side effect* on a captured lexical
    my @z__3 = (40); # BEGIN
    my $count = undef;  # $count is not initialized at compile-time
    my $v = undef;
    {
        my $z = shift @z__0;
        *z1 = sub  {
            eval ' print "in eval $z\\n" ';
            $z;
        };
    }
    {
        my $z = shift @z__1;
        *z2 = sub  {
            eval ' print "in eval $z\\n" ';
            $z;
        };
    }
    {
        my $z = shift @z__2;
        *z3 = sub  {
            eval ' print "in eval $z\\n" ';
            $z;
        };
    }
    *z0 = sub  {
            my $z = shift;
            # skip: * = sub ... *moved outside*
            # skip: BEGIN ... *moved outside*
            my $z = shift;
            # skip: * = sub ... *moved outside*
            # skip: BEGIN ... *moved outside*
            for my $z (32..35) {
                # skip: *z3 = sub  { eval ' print "in eval $z\\n" '; $z }
                # skip: BEGIN { $z = 9 }
                print "z3() ", z3(), "\n";
            }
        };
    # end of compile-time dump
    # start of run-time dump
    $v = 3;  # $v at run-time
    # skip: compile-time stuff
    print z1(), "\n"; # 5
    print z2(), "\n"; # 7
    print z3(), "\n"; # 9
    z0(10, 15);
    print z1(), "\n"; # 10
    print z2(), "\n"; # 15
    print z3(), "\n"; # 9
    z0(20, 25);
    print z1(), "\n"; # still 10
    print z2(), "\n"; # still 15
    print z3(), "\n"; # 9
    print $v, "\n"; # 3
    $count = 0;
    {
        # first run-time execution uses $z__3 instead of $z = undef
        my $z = shift @z__3;
        print "block $z\n";
        $z++ && redo
            if $count__0++ < 2;
    }


__END__

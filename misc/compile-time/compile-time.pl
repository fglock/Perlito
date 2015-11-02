use strict;

print "part 1\n";

{
    my $x = 10;
    sub x { $x; eval '$x' }
}

BEGIN {
    print x, "\n";  # nothing
}

print x, "\n";  # 10

#-----------------------

print "part 2\n";

sub y0 {
    my $y = shift;
    sub y1 { $y; eval '$y' }
}


print y1, "\n"; # nothing
y0(10);
print y1, "\n"; # 10
y0(20);
print y1, "\n"; # still 10

#-----------------------

print "part 3\n";

sub z0 {
    my $z = shift;
    sub z1 { $z; eval '$z' }
    BEGIN { $z = 5 }
}


print z1, "\n"; # 5
z0(10);
print z1, "\n"; # 10
z0(20);
print z1, "\n"; # still 10

#-----------------------

my @RUN;
push @RUN, sub {
    print "part 3 again\n";
};
{
    # $COMPILE::k;           # do: my ...
                    # skip: $k = shift;
    sub k1 { $COMPILE::k }   # do: sub ...
    $COMPILE::k = 5;         # do: BEGIN ... *side effect*
    sub k0 {
        *k0 = sub {
                my $k = shift;
                # skip: sub ... *moved outside*
                # skip: BEGIN ... *moved outside*

                eval '$k';
              };
        # skip: my ...
        $COMPILE::k = shift;
        # skip: sub ... *moved outside*
        # BEGIN executes here
        # skip: BEGIN side effect *moved outside*

        # eval '$k';    # TODO !!!
        eval '$COMPILE::k';
    }
}
push @RUN, sub {
    print k1, "\n"; # 5
    k0(10);
    print k1, "\n"; # 10
    k0(20);
    print k1, "\n"; # still 10
};
$_->() for @RUN;

#-----------------------

    #pragma: alias $COMPILE::f, $f
    # $COMPILE::f;           # do: my ...
                    # skip: $f = shift;
    sub f1 { $COMPILE::f }   # do: sub ...
    sub f0 {
        *f0 = sub {
                my $f = shift;
                # skip: sub ... *moved outside*
                # skip: BEGIN ... *moved outside*

                eval '$f';
              };
        # skip: my ...
        $COMPILE::f = shift;
        # skip: sub ... *moved outside*
        # BEGIN executes here
        # skip: BEGIN side effect *moved outside*

        # eval '$f';    # TODO !!!
        eval '$COMPILE::f';
    }
    $COMPILE::f = 5;         # do: BEGIN ... *side effect*  *moved down*
    #pragma: unalias $COMPILE::f, $f
    print "part 3 again, unrolled\n"; # *moved down*
    print f1, "\n"; # 5
    f0(10);
    print f1, "\n"; # 10
    f0(20);
    print f1, "\n"; # still 10

#-----------------------

    # fails to compile
    # print z1, "\n"; # 5
    # ---> No comma allowed after filehandle at x.pl line 2.

    print z1(), "\n"; # 5
    z0(10);
    print z1(), "\n"; # 10
    z0(20);
    print z1(), "\n"; # still 10
    sub z0 {
        my $z = shift;
        sub z1 { $z; eval '$z' }
        BEGIN { $z = 5 }
    }


    # original code
    sub z0 {
        my $z = shift;
        sub z1 { $z; eval '$z' }
        BEGIN { $z = 5 }
    }
    print z1, "\n"; # 5
    z0(10);
    print z1, "\n"; # 10
    z0(20);
    print z1, "\n"; # still 10

  
    # after compile-time env
    {
        my $g = 5;  # BEGIN ... *side effect* on a captured lexical

                        # skip: $g = shift;
        sub g1 { $g }   # do: sub ...
        sub g0 {
            *g0 = sub {
                    my $g = shift;
                    # skip: sub ... *moved outside*
                    # skip: BEGIN ... *moved outside*
                    eval '$g';
                  };
            # skip: my ...
            $g = shift;
            # skip: sub ... *moved outside*
            # BEGIN executes here
            # skip: BEGIN side effect *moved outside*
            eval '$g';
        }
    }
    print g1, "\n"; # 5
    g0(10);
    print g1, "\n"; # 10
    g0(20);
    print g1, "\n"; # still 10


#--------------------------

    # original code
    sub z0 {
        my $z = shift;
        sub z1 { eval ' print "in eval $z\\n" '; $z }
        BEGIN { $z = 5 }
    }
    print z1, "\n"; # 5
    z0(10);
    print z1, "\n"; # 10
    z0(20);
    print z1, "\n"; # still 10

  
    # after compile-time env
    {
        my $g = 5;  # BEGIN ... *side effect* on a captured lexical

                        # skip: $g = shift;
        sub g1 {
            eval ' print "in eval $g\\n" ';
            $g;
        }
        sub g0 {
            *g0 = sub {
                    my $g = shift;
                    # skip: sub ... *moved outside*
                    # skip: BEGIN ... *moved outside*
                  };
            # skip: my ...
            $g = shift;
            # skip: sub ... *moved outside*
            # BEGIN executes here
            # skip: BEGIN side effect *moved outside*
        }
    }
    print g1, "\n"; # 5
    g0(10);
    print g1, "\n"; # 10
    g0(20);
    print g1, "\n"; # still 10


    # using globals - FAIL

    $BEGIN_z = 5;
    sub z1 { eval ' print "in eval $BEGIN_z\\n" '; $BEGIN_z }
    sub z0 {
        my $z = shift;
    }
    print z1, "\n"; # 5
    z0(10);
    print z1, "\n"; # 10
    z0(20);
    print z1, "\n"; # still 10


#--------------------------

    # original code
    sub z0 {
        my $z = shift;
        sub z1 { eval ' print "in eval $z\\n" '; $z }
        BEGIN { $z = 5 }
        my $z = shift;      # redefine $z
        sub z2 { eval ' print "in eval $z\\n" '; $z }
        BEGIN { $z = 7 }
    }
    print z1, "\n"; # 5
    print z2, "\n"; # 7
    z0(10, 15);
    print z1, "\n"; # 10
    print z2, "\n"; # 15
    z0(20, 25);
    print z1, "\n"; # still 10
    print z2, "\n"; # still 15

  
    # after compile-time env
    {
        my $g__0 = 5;  # BEGIN ... *side effect* on a captured lexical
        my $g__1 = 7;  # BEGIN ... *side effect* on a captured lexical

                        # skip: $g = shift;
        sub g1 {
            eval ' print "in eval $g__0\\n" ';
            $g__0;
        }
        sub g2 {
            eval ' print "in eval $g__1\\n" ';
            $g__1;
        }
        sub g0 {
            *g0 = sub {
                    my $g = shift;
                    # skip: sub ... *moved outside*
                    # skip: BEGIN ... *moved outside*
                    my $g = shift;
                    # skip: sub ... *moved outside*
                    # skip: BEGIN ... *moved outside*
                  };
            # skip: my ...
            $g__0 = shift;
            # skip: sub ... *moved outside*
            # BEGIN executes here
            # skip: BEGIN side effect *moved outside*
            $g__1 = shift;
        }
    }
    print g1, "\n"; # 5
    print g2, "\n"; # 7
    g0(10, 15);
    print g1, "\n"; # 10
    print g2, "\n"; # 15
    g0(20, 25);
    print g1, "\n"; # still 10
    print g2, "\n"; # still 15

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
    $v__0 = undef;  # $v is not initialized at compile-time
    $z__0 = 5;  # BEGIN ... *side effect* on a captured lexical
    $z__1 = 7;  # BEGIN ... *side effect* on a captured lexical
    $z__2 = 9;  # BEGIN ... *side effect* on a captured lexical
    $z__3 = 40; # BEGIN
    $count__0 = undef;  # $count is not initialized at compile-time
    *z1 = sub  {
        eval ' print "in eval $z__0\\n" ';
        $z__0;
    };
    *z2 = sub  {
        eval ' print "in eval $z__1\\n" ';
        $z__1;
    };
    *z3 = sub  {
        eval ' print "in eval $z__2\\n" ';
        $z__2;
    };
    $RUN__0 = 1;
    *z0 = sub  {
        if ($RUN__0) {
            $RUN__0 = 0;
            # skip: my ...
            $z__0 = shift;
            # skip: sub ... *moved outside*
            # BEGIN executes here
            # skip: BEGIN side effect *moved outside*
            $z__1 = shift;
            # skip: sub ... *moved outside*
            for my $z (32..35) {    # note 'my' here - the for-loop variable is "localized"
                # skip: sub z3 { eval ' print "in eval $z\\n" '; $z }
                # skip: BEGIN { $z = 9 }
                print "z3() ", z3(), "\n";
            }
        }
        else {
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
    };
    # end of compile-time dump
    # start of run-time dump
    $v__0 = 3;  # $v at run-time
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
    print $v__0, "\n"; # 3
    $count__0 = 0;
    #
    # Note:
    # if the variable "$RUN__1" is false just after compile-time,
    # then the part "if ($RUN__1) {...}" can be removed as dead code
    #
    # Note:
    # "$RUN__1" is only needed if the block contains a BEGIN-ish expression:
    #   - BEGIN block
    #   - "use" statement
    #   - "sub NAME ..."
    #   - other special blocks: UNITCHECK, CHECK, INIT, END
    #
    # Note: set ${^GLOBAL_PHASE} accordingly
    #
    $RUN__1 = 1;
    {
        if ($RUN__1) {
            $RUN__1 = 0;
            # first run-time execution uses $z__3 instead of $z
            print "block $z__3\n";
            $z__3++ && redo
                if $count__0++ < 2;
        }
        else {
            my $z;
            print "block $z\n";
            $z++ && redo
                if $count__0++ < 2;
        }
    }


__END__

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
    # my @v__0 = (undef);      # $v is not initialized at compile-time
    my @z__3 = (40);         # BEGIN
    # my @count__0 = (undef);  # $count is not initialized at compile-time

    # runtime initialization skipped because the compile-time value is 'undef'
    # my $v = shift @v__0;
    # my $count = shift @count__0;

    # runtime
    my $v = 3;
    my $count = 0;
    {
        my $z = shift @z__3;
        print "block $z\n";
        $z++ && redo
            if $count++ < 2;
    }


__END__

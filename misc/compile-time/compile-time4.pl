use strict;
use warnings;
no warnings 'uninitialized';

{
    # original code
    my $v = 3;
    my $count = 0;
    for my $i (0..3)
    {
        my $z;
        BEGIN { $z = 40 }
        sub xyz { $z }
        print "block $z\n";
        $z++ && redo
            if $count++ < 2;
    }
    print "sub xyz: ", xyz(),"\n";
}
 
{ 
    # after compile-time env
    # 'my' variables in the compile-time scratchpad
    # my @v__0 = (undef);      # $v is not initialized at compile-time
    my @z__3 = (40);           # BEGIN
    # my @count__0 = (undef);  # $count is not initialized at compile-time
    # for my $i (0..3)
    {
        my $i;
        my $z = $z__3[0];      # $z is not shared
        sub xyz2 { $z }
    }

    # runtime initialization skipped because the compile-time value is 'undef'
    # my $v = shift @v__0;
    # my $count = shift @count__0;

    # runtime
    my $v = 3;
    my $count = 0;
    for my $i (0..3)
    {
        my $z = shift @z__3;
        print "block $z\n";
        $z++ && redo
            if $count++ < 2;
    }
    print "sub xyz2: ", xyz2(),"\n";
}

__END__

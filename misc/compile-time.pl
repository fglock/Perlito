use strict;

print "part 1\n";

{
    my $x = 10;
    sub x { $x }
}

BEGIN {
    print x, "\n";  # nothing
}

print x, "\n";  # 10

#-----------------------

print "part 2\n";

sub y0 {
    my $y = shift;
    sub y1 { $y }
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
    sub z1 { $z }
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
              };
        # skip: my ...
        $COMPILE::k = shift;
        # skip: sub ... *moved outside*
        # skip: BEGIN ... *moved outside*
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

__END__

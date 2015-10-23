use strict;

print "1..4\n";
my $v = 123;

{
    my $foo;
    $$foo[1] = $v;
    print "no" unless $foo->[1] == $v;
    print "ok 1\n";
}

{
    my $foo;
    $$foo{bar} = $v;
    print "no" unless $foo->{bar} == $v;
    print "ok 2\n";
}

{
    my $foo;
    ${ $$foo[1] }[2] = $v;
    print "no" unless $foo->[1]->[2] == $v;
    print "ok 3\n";
}

{
    my $foo;
    ${ $$foo[1] }[2] = $v;
    print "no" unless $foo->[1][2] == $v;
    print "ok 4\n";
}


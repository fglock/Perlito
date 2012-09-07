print "1..4\n";

eval 'print "ok 1 # say from eval\\n"';

eval '{;print "not "';
print "ok 2 # we live after evaling incorrect code\n";

# contributed by sbertrang++

sub foo {
    my $x = eval { return "bar" };
    return "baz-$x";
}
my $bar = foo();
print "not " if $bar ne "baz-bar";
print "ok 3 # return from eval block $bar\n";

sub foo2 {
    my $x = eval qq{ return "bar" };
    return "baz-$x";
}
my $bar2 = foo2();
print "not " if $bar2 ne "baz-bar";
print "ok 4 # return from eval string $bar2\n";


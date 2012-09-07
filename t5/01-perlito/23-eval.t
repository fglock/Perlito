use strict;
use feature 'say';

print "1..10\n";

eval 'print "ok 1 # say from eval\\n"';

eval '{;print "not "';
print "ok 2 # we live after evaling incorrect code\n";

# contributed by sbertrang++

sub foo {
    my $x = eval { return "bar" };
    print "# err $@\n" if $@;
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



# this code from Abigail++


eval {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    print "not " if $result ne 'SCALAR';
    say "ok 5 # Eval block: $result";
    1;
} or do {warn "Oops"};


eval <<'--' or do {warn "Oops"};
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    print "not " if $result ne 'SCALAR';
    say "ok 6 # Eval string: $result";
    1;
--


sub foo3 {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    print "not " if $result ne 'SCALAR';
    say "ok 7 # Sub: $result";
    1;
}

foo3 or do {warn "Oops"};


sub {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    print "not " if $result ne 'SCALAR';
    say "ok 8 # Anon: $result";
    1;
} -> () or do {warn "Oops"};



# Now, note that if the eval is in list context, wantarray pick that up:

    
sub foo4 {1;}

foo4 (eval {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    print "not " if $result ne 'LIST';
    say "ok 9 # Eval: $result";
    1;
});



# And if the eval is in void context, wantarray picks that up as well:

eval {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    print "not " if $result ne 'VOID';
    say "ok 10 # Eval: $result";
    1;
};



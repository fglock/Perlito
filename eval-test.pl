# this code from Abigail++

use feature 'say';

eval {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    say "Eval block: $result";
    1;
} or do {warn "Oops"};


eval <<'--' or do {warn "Oops"};
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    say "Eval string: $result";
    1;
--


sub foo {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    say "Sub: $result";
    1;
}

foo or do {warn "Oops"};


sub {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    say "Anon: $result";
    1;
} -> () or do {warn "Oops"};



# Now, note that if the eval is in list context, wantarray pick that up:

    
sub foo {1;}

foo (eval {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    say "Eval: $result";
    1;
});



# And if the eval is in void context, wantarray picks that up as well:

eval {
    my $result = wantarray ? "LIST" : defined wantarray ? "SCALAR" : "VOID";
    say "Eval: $result";
    1;
};




__END__
Eval block: SCALAR
Eval string: SCALAR
Sub: SCALAR
Anon: SCALAR

Eval block: LIST

Eval: VOID

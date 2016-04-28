use feature 'say';
use feature 'state';
use Data::Dumper;

sub reporter {
    my $testname = shift || '<unnamed-test>';
    my $testnum = 1;
    return sub {
        my $ok = shift;
        my $name = shift || $testname;
        my $extra_msg = join ' ', @_;
        my $msg = ($ok ? "" : "not ") . "ok $testnum - $name";
        $msg .= $extra_msg ? ": $extra_msg." : ".";
        ++$testnum;
        say $msg;
    };
}

my $t = reporter();
my $test_number = 1;
# State with const init.
sub ctr {
    state $x = 0;
    $x++;
}

for (1..3) {
    $t->($_ == ctr() + 1, "state with constant init");
}

#say '-' x 80;

for (1..3) {
    sub foo {
        state $x = 0;
        $x++;
    }
    $t->($_ == foo() + 1, "state in a sub within a loop");
}

#say '-' x 80;

for (1..3) {
    my $bar = sub {
        state $x = 0;
        $x++;
    };
    $t->($bar->() == 0, "state in a closure recreated everytime in a loop");
}


for (1..3) {
    sub foos {
        state $x = $_;
        $x++;
    }
    my $val = foos();
    $t->($val == $_, 'state in a sub within a loop initialized with $_');
}

my $i = 10;
for (10,20,30,40) {
    sub spam {
        my $x = (state $y = $_[0]);
        ($x++, $y++);
    }
    my ($x, $y) = spam($_);
    $t->($x == $i && $y == $i, 'my variable initialized from a state expr');
    ++$i;
}

sub ding {
    state $x;
    ($x) = @_;
    return ++$x;
}

for (10,20,30) {
    $t->(ding($_) == $_ + 1, 'state variable without initialization but modified later');
}

sub convoluted_counter {
    my $ctr = do {
        state $x = $_[0];
        sub {
            ++$x;
        }
    };
    $ctr->();
}

# This function uses a state variable that starts out as undef and calls
# post-increment on it, returning the value of the expression. When this
# function is called, the value returned from the first call is 0 and not undef.
sub uninitialized_state {
    state $x;
    $x++;
}

my @result;
push @result, uninitialized_state for (1..3);
my @expected = (0, 1, 2);
for my $i (0..$#expected) {
    $t->($result[$_] == $expected[$_], 'Uninitialized state variable with post increment operator');
}

my $convoluted_counter_test = 'state variable in a do block that returns a sub closing over the state variable';
$t->(convoluted_counter(0) == 1, $convoluted_counter_test);
$t->(convoluted_counter(100) == 2, $convoluted_counter_test);
$t->(convoluted_counter(5) == 3, $convoluted_counter_test);
$t->(convoluted_counter == 4, $convoluted_counter_test);


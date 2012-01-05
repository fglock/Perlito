package Perlito5::Test;

### GLOBALS

# globals to keep track of our tests
my $num_of_tests_run;
my $num_of_tests_failed;
my $num_of_tests_badpass;
my $num_of_tests_planned;

# for running the test suite multiple times in the same process
my $testing_started;

### FUNCTIONS

sub plan {
    my $number_of_tests = shift;
    $testing_started      = 1;
    $num_of_tests_planned = $number_of_tests;

    say "1..$number_of_tests";
}

sub ok {
    my $cond    = shift;
    my $desc    = shift;
    my $todo    = shift;
    my $depends = shift;
    Perlito5::Test::proclaim( $cond, 'ok! ' . $desc, $todo, $depends );
}

sub is {
    my $got      = shift;
    my $expected = shift;
    my $desc     = shift;
    my $todo     = shift;
    my $depends  = shift;

    my $test = $got eq $expected;
    Perlito5::Test::proclaim( $test, 'is! ' . $desc, $todo, $got, $expected, $depends );
}

sub is_deeply {
    my $got      = shift;
    my $expected = shift;
    my $desc     = shift;
    my $todo     = shift;
    my $depends  = shift;

    # hack for now - TODO - use Data::Dumper
    my $got_perl      = $got;
    my $expected_perl = $expected;
    my $test          = ( $got_perl eq $expected_perl );

    Perlito5::Test::proclaim( $test, "is deeply! $desc", $todo, $got_perl, $expected_perl, $depends );
}

sub isnt {
    my $got      = shift;
    my $expected = shift;
    my $desc     = shift;
    my $todo     = shift;
    my $depends  = shift;

    my $test = !( $got eq $expected );
    Perlito5::Test::proclaim( $test, "isnt! $desc", $todo, $got, $expected, $depends, { negate => 1 } );
}

sub cmp_ok {
    my $got          = shift;
    my $compare_func = shift;
    my $expected     = shift;
    my $desc         = shift;
    my $todo         = shift;
    my $depends      = shift;

    say "### Perlito5::Test::cmp_ok not implemented";
}

sub like {
    say "### Perlito5::Test::like not implemented";
}

sub unlike {
    say "### Perlito5::Test::unlike not implemented";
}

sub eval_dies_ok {
    say "### Perlito5::Test::eval_dies_ok not implemented";
}

sub isa_ok {
    say "### Perlito5::Test::isa_ok not implemented";
}

sub use_ok {
    say "### Perlito5::Test::use_ok not implemented";
}

sub throws_ok {
    say "### Perlito5::Test::throws_ok not implemented";
}

sub dies_ok {
    say "### Perlito5::Test::dies_ok not implemented";
}

sub lives_ok {
    say "### Perlito5::Test::lives_ok not implemented";
}

## misc. test utilities

sub skip {
    my $reason  = shift;
    my $depends = shift;
    Perlito5::Test::proclaim( 1, '', "skip " . $reason, $depends );
}

sub pass {
    my $desc = shift;
    Perlito5::Test::proclaim( 1, 'pass! ' . $desc );
}

sub flunk {
    my $desc    = shift;
    my $todo    = shift;
    my $depends = shift;

    Perlito5::Test::proclaim( 0, 'flunk! ' . $desc, $todo, $depends );
}

## 'private' subs

sub proclaim {
    my $cond     = shift;
    my $desc     = shift;
    my $todo     = shift;
    my $got      = shift;
    my $expected = shift;
    my $depends  = shift;
    my $negate   = shift;

    $testing_started  = 1;
    $num_of_tests_run = $num_of_tests_run + 1;

    if ($cond) {
        say "ok ", $num_of_tests_run;
    }
    else {
        say "not ok ", $num_of_tests_run;
        Perlito5::Test::report_failure( $todo, $got, $expected, $negate );
    }

    return $cond;
}

sub report_failure {
    my $todo     = shift;
    my $got      = shift;
    my $expected = shift;
    my $negate   = shift;

    say "### Perlito5::Test::report_failure not implemented";
}

sub test_ends {
    if ( !$testing_started ) {
        return;
    }

    if ( !$num_of_tests_planned ) {
        say "1.." . $num_of_tests_run;
    }

    if ( $num_of_tests_planned != $num_of_tests_run ) {
        say "# Looks like you planned " . $num_of_tests_planned . " tests, but ran " . $num_of_tests_run;
    }

    if ($num_of_tests_failed) {
        say "# Looks like you failed " . $num_of_tests_failed . " tests of " . $num_of_tests_run;
    }

    $num_of_tests_run     = 0;
    $num_of_tests_failed  = 0;
    $num_of_tests_planned = 0;
    $testing_started      = 0;
}


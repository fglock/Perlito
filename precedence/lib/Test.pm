class Test {

  ### GLOBALS

  # globals to keep track of our tests
  my $num_of_tests_run;
  my $num_of_tests_failed;
  my $num_of_tests_badpass;
  my $num_of_tests_planned;

  # for running the test suite multiple times in the same process
  my $testing_started;

### FUNCTIONS

  sub plan ($number_of_tests) {
    $testing_started      = 1;
    $num_of_tests_planned = $number_of_tests;

    say '1..' ~ $number_of_tests;
  }


  sub ok ($cond, $desc, $todo, $depends) {
    Test::proclaim($cond, 'ok! ' ~ $desc, $todo, $depends);
  }


  sub is ($got, $expected, $desc, $todo, $depends) {
    my $test = $got eq $expected;
    Test::proclaim($test, 'is! ' ~ $desc, $todo, $got, $expected, $depends);
  }


  sub is_deeply($got, $expected, $desc, $todo, $depends) {
    # hack for now
    my $got_perl      = $got.perl;
    my $expected_perl = $expected.perl;
    my $test          = ($got_perl eq $expected_perl);

    Test::proclaim($test, 'is deeply! ' ~ $desc, $todo, $got_perl, $expected_perl, $depends);
  }


  sub isnt ($got, $expected, $desc, $todo, $depends) {
    my $test = !($got eq $expected);
    Test::proclaim($test, 'isnt! ' ~ $desc, $todo, $got, $expected, $depends, { negate => 1 });
  }


  sub cmp_ok ($got, &compare_func, $expected, $desc, $todo, $depends) {
    say "### Test::cmp_ok not implemented";
  }


  sub like () {
    say "### Test::like not implemented";
  }

  sub unlike () {
    say "### Test::unlike not implemented";
  }

  sub eval_dies_ok () {
    say "### Test::eval_dies_ok not implemented";
  }

  sub isa_ok () {
    say "### Test::isa_ok not implemented";
  }

  sub use_ok () {
    say "### Test::use_ok not implemented";
  }

  sub throws_ok () {
    say "### Test::throws_ok not implemented";
  }

  sub dies_ok () {
    say "### Test::dies_ok not implemented";
  }

  sub lives_ok () {
    say "### Test::lives_ok not implemented";
  }

## misc. test utilities

  sub skip ($reason, $depends) {
    Test::proclaim(1, '', "skip " ~ $reason, $depends);
  }

  sub pass ($desc) {
    Test::proclaim(1, 'pass! ' ~ $desc);
  }

  sub flunk ($desc, $todo, $depends) {
    Test::proclaim(0, 'flunk! ' ~ $desc, $todo, $depends);
  }


## 'private' subs

  sub proclaim ($cond, $desc, $todo, $got, $expected, $depends, $negate) {
    $testing_started  = 1;
    $num_of_tests_run = $num_of_tests_run + 1;

    if ( $cond ) {
      say "ok ", $num_of_tests_run;
    } else {
      say "not ok ", $num_of_tests_run;
      Test::report_failure($todo, $got, $expected, $negate);
    }

    return $cond;
  }


  sub report_failure ($todo, $got, $expected, $negate) {
    say "### Test::report_failure not implemented";
  }


  sub test_ends {
    if ( !$testing_started ) {
      return;
    }

    if (!$num_of_tests_planned) {
        say "1.." ~ $num_of_tests_run;
    }

    if ($num_of_tests_planned != $num_of_tests_run) {
        say "# Looks like you planned " ~ $num_of_tests_planned ~ " tests, but ran " ~ $num_of_tests_run;
    }

    if ($num_of_tests_failed) {
        say "# Looks like you failed " ~ $num_of_tests_failed ~ " tests of " ~ $num_of_tests_run;
    }

    $num_of_tests_run     = 0;
    $num_of_tests_failed  = 0;
    $num_of_tests_planned = 0;
    $testing_started      = 0;
  }

}

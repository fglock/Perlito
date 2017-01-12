use v5;
use strict;
use feature 'say';

say '1..3';

# stackoverflow.com/questions/161872/hidden-features-of-perl
#
# The "desperation mode" of Perl's loop control constructs which causes them 
# to look up the stack to find a matching label allows some curious behaviors 
# which Test::More takes advantage of, for better or worse.

sub skip {
    no warnings "exiting";
    last SKIP;
}

my $something = 1;

{
  SKIP: {
        say "ok 1";
        skip() if $something;

        say "not ok 1";
    }
}

$something = 0;

{
  SKIP: {
        skip() if $something;

        say "ok 2";
    }
}

# interaction between do() and last()

sub skip_do {
    no warnings "exiting";
    do { last SKIP2 };
    return;
}

$something = 1;

{
  SKIP2: {
        say "ok 3";
        skip_do() if $something;

        say "not ok 3";
    }
}



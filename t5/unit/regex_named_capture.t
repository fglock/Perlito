use v5;
use feature 'say';
use strict;
use warnings;

say '1..2';

my $test = 'abc1234asf';

$test =~ /(?<one>\w{4})(?<two>\w{4})/;

my $one =  $+{one};
my $two =  $+{two};

print "not " unless $one eq "abc1";
say 'ok 1';

print "not " unless $two eq "234a";
say 'ok 2';


use v5;
use feature 'say';
use strict;
use warnings;

say '1..3';

my $test = 'abc1234asf';

$test =~ /(?<one>\w{4})(?<two>\w{4})/;

my $one =  $+{one};
my $two =  $+{two};

print "not " unless $one eq "abc1";
say 'ok 1';

print "not " unless $two eq "234a";
say 'ok 2';


$test =~ /(?<_99>\w{4})/;

$one =  $+{_99};

print "not " unless $one eq "abc1";
say 'ok 3 - underscore in name';



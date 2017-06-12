use v5.26;
use feature 'say';
use strict;
use warnings;

say '1..2';

my $str = "Perl";

print "not " unless $str =~ /P [e] rl/xx;
say 'ok 1';

$str = "P e r l";
print "not " if $str =~ /P [ x] e/xx;
say 'ok 2';


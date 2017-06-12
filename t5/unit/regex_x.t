use v5;
use feature 'say';
use strict;
use warnings;

say '1..2';

my $str = "Perl";

print "not " unless $str =~ /P [e] rl/x;
say 'ok 1';

$str = "P e r l";
print "not " unless $str =~ /P [ e] e/x;
say 'ok 2';



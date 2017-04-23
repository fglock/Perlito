use v5;
use feature 'say';
use strict;
use warnings;

say '1..4';

my $str = "Perl";

print "not " unless $str =~ /\120/;    # Match, "\120" is "P".
say 'ok 1';

print "not " unless $str =~ /\120+/;   # Match, "\120" is "P", it is repeated at least once.
say 'ok 2';

print "not " if $str =~ /P\053/;   # No match, "\053" is "+" and taken literally.
say 'ok 3';

my $str = "Pe/rl";
print "not " unless $str =~ m{Pe/};   # test slash in JS
say 'ok 4';



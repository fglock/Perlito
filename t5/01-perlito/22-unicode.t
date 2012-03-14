use v5;
use strict;
use feature 'say';

use Perlito5::Test;

Perlito5::Test::plan 1;

my $s1 = "a≤b"; 
my $s2 = "a\c[8804]b";

print "not " if $s1 ne $s2;
say "ok 1 - unicode char by number: '$s1' eq '$s2'";


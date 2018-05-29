
use v6-perlito;
use Perlito6::Test;

Perlito6::Test::plan 1;



my $s1 = "a≤b"; 
my $s2 = "a\c[8804]b";

print "not " if $s1 ne $s2;
say "ok 1 - unicode char by number: '$s1' eq '$s2'");


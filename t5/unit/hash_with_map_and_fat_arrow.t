#!./perl -w

use feature 'say';
print "1..3\n";

my %x = (map { $_ => 1 } qw(one two three));

print "not " if $x{'one'} != 1;
say "ok 1 - map+fatarrow x{one}";

print "not " if $x{'three'} != 1;
say "ok 2 - map+fatarrow: x{three}";

print "not " if exists($x{'four'});
say "ok 3 - x{four} does not exist";


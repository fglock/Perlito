use feature "say";
use strict;

say "1..1";

# A list assignment in scalar context returns the number of elements on the right hand side:
my $count = scalar(my ($hello, $there, $world) = (7,8));
print "not " if $count != 2;
say "ok 1 # $count";



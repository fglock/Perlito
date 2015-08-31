use v5;
use strict;
use feature 'say';

say '1..1';
my $v;

$v = eval '013.4e2';
print 'not ' if $v != 11400;
say 'ok 1 - octal integer followed by dot';


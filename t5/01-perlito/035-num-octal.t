use v5;
use strict;
use feature 'say';

say '1..3';
my $v;

$v = eval '013.4e2';
print 'not ' if $v != 11400;
say 'ok 1 - octal integer followed by dot # $v';

my $oct = '013.4e2';
$v = 0 + $oct;
print 'not ' if $v != 1340;
say "ok 2 - non-octal integer followed by dot # $v";

$oct = '013';
$v = 0 + $oct;
print 'not ' if $v != 13;
say "ok 3 - non-octal integer # $v";



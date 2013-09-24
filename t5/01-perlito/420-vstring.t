use v5;
use strict;
use feature 'say';

say "1..3";
my $v;

eval ' $a = v100; ';
$v = ref(\$a);
print "not " if $v ne 'VSTRING';
say "ok 1 - $v # TODO";

sub v200 { 123 }
$a = v200; 
$v = ref(\$a);
print "not " if $v ne 'SCALAR';
say "ok 2 - $v";

$a = v200.200; 
$v = ref(\$a);
print "not " if $v ne 'VSTRING';
say "ok 3 - $v # TODO";


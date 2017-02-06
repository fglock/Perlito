use feature 'say';
use 5.18.0;
use strict;
use warnings;

say '1..2';

my $str1 = "Hello";
my $str2 = "heLLo";
my $x = fc $str1;
my $y = fc $str2;

if ($x ne $y) {
    print 'not ';
}
say "ok 1 - '$x' == '$y'";

$str1 = "Just something";
$str2 = "Anything else";

$x = fc $str1;
$y = fc $str2;
if ($x eq $y) {
    print 'not ';
}
say "ok 2 - '$x' ne '$y'";


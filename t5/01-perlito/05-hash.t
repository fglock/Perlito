use v5;
use strict;
use feature 'say';

say '1..11';
my %a;
say 'ok 1 - create hash';
$a{abc} = 3;
say 'ok 2 - set element';
if ($a{abc} != 3) {
    print 'not '
}
say 'ok 3 - fetch element # ', $a{abc};

$a{123} = 456;
say '# values: ', values %a;
say '# keys:   ', keys %a;

my %a1 = (a => 2); 
if ($a1{a} ne 2) {
    print 'not '
}
say "ok 4 - assign list to hash # {%a1}";

my %b1 = %a1; 
if ($b1{a} ne 2) {
    print 'not '
}
say "ok 5 - assign hash to hash # {%b1}";
$b1{a} = 5;
print 'not '
    unless $a1{a} == 2;
say "ok 6 - hash copy";
print 'not '
    unless $b1{a} == 5;
say "ok 7 - hash copy";
$b1{a} = 2;


my $c1 = { %b1, b => 3 };
if ($c1->{a} ne 2 || $c1->{b} ne 3) {
    print 'not '
}
say "ok 8 - interpolate hash in hash composer "; # {$c1};

print 'not ' if defined $c1->{c};
say "ok 9 - undefined item";

print 'not ' if !defined $c1->{b};
say "ok 10 - defined item";

$c1->{c} = 4;
print 'not ' if !defined $c1->{c};
say "ok 11 - defined item";



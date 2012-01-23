use v5;
use strict;
use feature 'say';

say '1..15';
my @a;
say 'ok 1 - create array';
$a[1] = 3;
say 'ok 2 - set element';
if ($a[1] != 3) {
    print 'not '
}
say 'ok 3 - fetch element # ', $a[1];

my @x = ( 3, 4 ); 
@a = ( 1, @x, 2 ); 
if ($a[1] != 3) {
    print 'not '
}
say 'ok 4 - interpolate array # ', @a;

my $x = [ 5, 6 ];
my $v = $x->[1];
if ($v != 6) {
    print 'not '
}
say 'ok 5 - array in a scalar var # ', $v;
$x->[1] = 7;
if ($x->[1] != 7) {
    print 'not '
}
say 'ok 6 - array in a scalar var # ', $x->[1];

{
    my $v;
    $v->[2] = 8;
    if ($v->[2] != 8) {
        print 'not '
    }
    say 'ok 7 - array in a scalar var # ', $v->[2];
}

print 'not ' if defined $x->[4];
say "ok 8 - undefined item";

print 'not ' if !defined $x->[1];
say "ok 9 - defined item";

$x->[4] = 5;
print 'not ' if !defined $x->[4];
say "ok 10 - defined item";

unshift(@$x, 6);
print 'not ' if $x->[0] != 6;
say "ok 11 - unshift";
print 'not ' if $x->[5] != 5;
say "ok 12 - unshift";

my @x13 = ( 3, 4 );
my $s13 = join('#', @x13);
print 'not '
    unless $s13 eq '3#4';
say "ok 13 - join # '$s13'";

my @x14 = @x13;
$x14[1] = 5;
print 'not '
    unless $x13[1] == 4;
say "ok 14 - array copy";
print 'not '
    unless $x14[1] == 5;
say "ok 15 - array copy";


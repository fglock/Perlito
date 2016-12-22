
use feature 'say';
# use strict;

say "1..6";

sub x { $_[0] = 3 }

my @a;
my $v;
$v = ${$a[2]};
print "not " if defined $v; say "ok 1 - $v";
$v = $a[2];
print "not " if defined $v; say "ok 2 - $v";

x(${$a[2]});
$v = ${$a[2]};
print "not " if $v != 3; say "ok 3 - $v";
$v = $a[2];
print "not " if !ref($v); say "ok 4 - $v " . ref($v);

sub z { $_[0]++ }

@a = ();
z(${$a[2]});
$v = ${$a[2]};
print "not " if $v != 1; say "ok 5 - $v";
$v = $a[2];
print "not " if !ref($v); say "ok 6 - $v " . ref($v);


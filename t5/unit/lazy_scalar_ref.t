
use feature 'say';
# use strict;

say "1..17";

sub x { $_[0] = 3 }

my $v;
my $a;
$v = $$a;
print "not " if defined $v; say "ok 1 - $v";
$v = $a;
print "not " if defined $v; say "ok 2 - $v";

x($$a);
$v = $$a;
print "not " if $v != 3; say "ok 3 - $v";
$v = $a;
print "not " if !ref($v); say "ok 4 - $v " . ref($v);

sub z { $_[0]++ }

$a = undef;
z($$a);
$v = $$a;
print "not " if $v != 1; say "ok 5 - $v";
$v = $a;
print "not " if !ref($v); say "ok 6 - $v " . ref($v);

sub t { $_[0] . "" }

$a = undef;
t($$a);
$v = $a;
print "not " if !defined $v; say "ok 7 - $v " . ref($v);

sub u { $_[0] }

$a = undef;
u($$a);
$v = $a;
print "not " if !defined $v; say "ok 8 - $v " . ref($v);

# 2-level

$a = undef;
$v = $$$a;
print "not " if defined $v; say "ok 9 - $v";
$v = $$a;
print "not " if defined $v; say "ok 10 - $v";
$v = $a;
print "not " if defined $v; say "ok 11 - $v";

$a = undef;
z($$$a);
$v = $$$a;
print "not " if $v != 1; say "ok 12 - $v";
$v = $$a;
print "not " if ref($v) ne 'SCALAR'; say "ok 13 - $v";
$v = $a;
print "not " if ref($v) ne 'REF'; say "ok 14 - $v " . ref($v);

$a = undef;
u($$$a);
$v = $$$a;
print "not " if $v; say "ok 15 - $v";
$v = $$a;
print "not " if ref($v) ne 'SCALAR'; say "ok 16 - $v";
$v = $a;
print "not " if ref($v) ne 'REF'; say "ok 17 - $v " . ref($v);



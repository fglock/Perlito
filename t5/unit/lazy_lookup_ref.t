
use feature 'say';
# use strict;

say "1..8";

sub x { $_[0] = 3 }

my %a;
my $v;
$v = ${$a{hh}};
print "not " if defined $v; say "ok 1 - $v";
$v = $a{hh};
print "not " if defined $v; say "ok 2 - $v";

x(${$a{hh}});
$v = ${$a{hh}};
print "not " if $v != 3; say "ok 3 - $v";
$v = $a{hh};
print "not " if !ref($v); say "ok 4 - $v " . ref($v);

sub z { $_[0]++ }

%a = ();
z(${$a{hh}});
$v = ${$a{hh}};
print "not " if $v != 1; say "ok 5 - $v";
$v = $a{hh};
print "not " if !ref($v); say "ok 6 - $v " . ref($v);

sub t { $_[0] }

%a = ();
t(${$a{hh}});
$v = ${$a{hh}};
print "not " if defined $v; say "ok 7 - $v";
$v = $a{hh};
print "not " if !ref($v); say "ok 8 - $v " . ref($v);


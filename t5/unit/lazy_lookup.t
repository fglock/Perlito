
use feature 'say';
# use strict;

say "1..9";

sub x { $_[0] = 3 }

my %a;
my $v;
$v = $a{hh};
print "not " if defined $v; say "ok 1 - $v";
$v = exists $a{hh};
print "not " if $v; say "ok 2 - $v";

x($a{hh});
$v = $a{hh};
print "not " if $v != 3; say "ok 3 - $v";
$v = exists $a{hh};
print "not " if !$v; say "ok 4 - $v";

sub z { $_[0]++ }

%a = ();
z($a{hh});
$v = $a{hh};
print "not " if $v != 1; say "ok 5 - $v";
$v = exists $a{hh};
print "not " if !$v; say "ok 6 - $v";

sub t { $_[0] . "" }

@a = ();
t($a{aa});
$v = exists $a{aa};
print "not " if $v; say "ok 7 - $v";

# 2-level

@a = ();
t($a{gg}{hh});
$v = exists $a{gg}{hh};
print "not " if $v; say "ok 8 - $v";
$v = exists $a{gg};
print "not " if !$v; say "ok 9 - $v";


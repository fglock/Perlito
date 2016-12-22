
use feature 'say';

say "1..4";

sub x { $_[0] = 3 }

my $v;
$v = $$a;
print "not " if defined $v; say "ok 1 - $v";
$v = $a;
print "not " if defined $v; say "ok 2 - $v";

x($$a);
$v = $$a;
print "not " if $v != 3; say "ok 3 - $v";
$v = $a;
print "not " if !ref($v); say "ok 4 - $v " . ref($v);


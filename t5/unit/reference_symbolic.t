use feature 'say';

say '1..3';

our $v = 123;
my $z;
$z = "v";
print 'not ' unless $$z eq "123";
say "ok 1 - symbolic reference, our # $$z";


$main::x = 999;
my $x = 124;
my $z;
$z = "x";
print 'not ' if $$z eq "124";
say "ok 2 - symbolic reference, my # $$z";

$z = "main::x";
print 'not ' unless $$z eq "999";
say "ok 3 - symbolic reference, fully qualified global # $$z";


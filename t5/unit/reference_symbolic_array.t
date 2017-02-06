use feature 'say';

say '1..3';

our @v = (123,456);
my $z;
$z = "v";
print 'not ' unless "@$z" eq "123 456";
say "ok 1 - symbolic reference, our # @$z";


@main::x = (999, 1000);
my @x = (124,457);
my $z;
$z = "x";
print 'not ' if "@$z" eq "124 457";
say "ok 2 - symbolic reference, my # @$z";

$z = "main::x";
print 'not ' unless "@$z" eq "999 1000";
say "ok 3 - symbolic reference, fully qualified global # @$z";


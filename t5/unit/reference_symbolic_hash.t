use feature 'say';

say '1..3';

our %v = (123,456);
my $z;
$z = "v";
my @out = %$z;
print 'not ' unless "@out" eq "123 456";
say "ok 1 - symbolic reference, our # @out";


%main::x = (999, 1000);
my %x = (124,457);
my $z;
$z = "x";
@out = %$z;
print 'not ' if "@out" eq "124 457";
say "ok 2 - symbolic reference, my # @out";

$z = "main::x";
@out = %$z;
print 'not ' unless "@out" eq "999 1000";
say "ok 3 - symbolic reference, fully qualified global # @out";


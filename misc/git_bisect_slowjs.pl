use strict;
use Data::Dumper;

`make build-5js`;
# `make boot-5js`;
my @v = `make boot-5js 2>&1`;
# print Dumper \@v;

my ($time) = grep { /^user\s+(\d)m(\d+)\./ } @v;
my @time = $time =~ /(\d)m(\d+)\./;
my $t = $time[0] * 60 + $time[1];

print STDERR "t = $t\n";

# good 0
# bad  1
# skip 125

exit(1) if $t > 26;
exit(125) if $t < 3;
exit(0);


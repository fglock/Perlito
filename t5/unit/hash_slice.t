use v5;
use strict;
use feature 'say';

say '1..2';

my %hash_slice;
my $res;

# http://perltricks.com/article/92/2014/5/27/Perl-v5-20-what-you-need-to-know

my %raindrops = ( splish => 4, splash => 9, splosh => 7 );
%hash_slice = %raindrops{ 'splish', 'splosh'};
# hash_slice is (splish => 4, splosh => 7)

$res = "@{[ %hash_slice ]}";
print 'not '
    if $res ne 'splish 4 splosh 7'
    && $res ne 'splosh 7 splish 4';
say "ok 1 - hash_slice # $res";

my @raindrop_types = qw/splish splash splosh/;
%hash_slice = %raindrop_types[0, 2];
# hash_slice is (0 => 'splish', 2 => 'splosh')

$res = "@{[ %hash_slice ]}";
print 'not '
    if $res ne '0 splish 2 splosh'
    && $res ne '2 splosh 0 splish';
say "ok 2 - hash_slice # $res";


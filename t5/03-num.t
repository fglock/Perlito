use v5;
use strict;
use feature 'say';

package Main;
say '1..8';
my $v = 1 + 0.3;
if (( $v < 1.29 ) || ( $v > 1.31 )) {
    print 'not '
}
say 'ok ', 1;

if (( $v . '' ) ne '1.3') {
    print 'not '
}
say 'ok ', 2, ' # ', $v;

if (( $v + '3.4' ) ne '4.7') {
    print 'not '
}
say 'ok ', 3, ' # ', ($v + '3.4');

if (( $v / 2 ) != 0.65) {
    print 'not '
}
say 'ok 4 # ', ($v / 2);

if (( $v * 2 ) != 2.6) {
    print 'not '
}
say 'ok 5 # ', ($v * 2);

print 'not ' if !defined 3.14;
say 'ok 6 - defined num';

my $v = 3.14;
print 'not ' if !defined $v;
say 'ok 7 - defined var';

my $y;
print 'not ' if defined $y;
say 'ok 8 - undefined var';


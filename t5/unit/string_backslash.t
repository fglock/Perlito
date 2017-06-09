use v5;
use strict;
use feature 'say';

say '1..2';

my $x = "abc\d";
if ( $x ne "abcd" ) {
    print 'not '
};
say 'ok 1 - double quote # ', $x;

$x = 'abc\d';
if ( $x ne "abc\\d" ) {
    print 'not '
};
say 'ok 2 - single quote # ', $x;;



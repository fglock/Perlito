use v5;
use strict;
use feature 'say';

say '1..3';

my $a = sub { 3 };
if ( defined prototype($a) ) {
    print 'not ';
}
say 'ok 1 - no prototype';

$a = sub () { 4 };
if ( !defined prototype($a) || prototype($a) ne '') {
    print 'not ';
}
say 'ok 2 - empty prototype # "', prototype($a), '"';

$a = sub ($$) { 5 };
if ( !defined prototype($a) || prototype($a) ne '$$') {
    print 'not ';
}
say 'ok 3 - prototype # "', prototype($a), '"';


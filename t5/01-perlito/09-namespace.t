use v5;
use strict;
use feature 'say';

package Main;

say '1..6';

sub subr { $_[0] + $_[1] };

package Mod2;
sub subr { $_[0] + $_[1] + 1 }

package Main;

my $x = 0;
$x = subr( 1, 2 );
if ($x != 3) {
    print 'not '
};
say 'ok 1 - ', $x;

sub subr3 { $_[0][0] + $_[0][1] }

$x = 0;
$x = subr3( [3, 4] );
if ($x != 7) {
    print 'not '
}
say 'ok 2 - ', $x;

# we are in the Main namespace

$x = 0;
$x = Main::subr( 1, 2 );
if ($x != 3) {
    print 'not '
}
say 'ok 3 - ', $x;

if (Mod2::subr( 1, 2 ) != 4) {
    print 'not '
}
say 'ok 4 - ', Mod2::subr( 1, 2 );

*subr4 = sub { 123 };

print "not " unless subr4() == 123;
say "ok 5";

*Mod2::subr4 = sub { 456 };

print "not " unless Mod2::subr4() == 456;
say "ok 6";


use v5;
use strict;
use feature 'say';

package Main;

say '1..2';

sub subr {
    my $self = shift;
    my $a = shift;
    my $b = shift;
    $a + $b;
}

my $x = 0;
$x = Main->subr( 1, 2 );
if ( $x != 3 ) {
    print 'not ';
}
say 'ok 1 - ', $x;

sub subr2 {
    my $self = shift;
    my $a = shift;
    my $b = shift;
    my $c = shift;
    $a + $b + $c;
}

$x = 0;
$x = Main->subr2( 1, 2, 4 );
if ( $x != 7 ) {
    print 'not ';
}
say 'ok 2 - ', $x;


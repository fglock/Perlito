use v5;
use strict;
use feature 'say';

package Main;
    say '1..5';
    my $a = sub { 3 };
    say 'ok 1 - create function';
    if ($a->() != 3) {
        print 'not '
    }
    say 'ok 2 - apply';
    $a = ( sub { 4 } )->();
    if ($a != 4) {
        print 'not '
    }
    say 'ok 3 - apply in line';
    $a = ( sub () { sub { 5 } } )->();
    if ($a->() != 5) {
        print 'not '
    }
    say 'ok 4 - return function';


{
    use feature 'current_sub';

    my $factorial = sub {
        my ($x) = @_;
        return 1 if $x == 1;
        return ( $x * __SUB__->( $x - 1 ) );
    };

    my $res = $factorial->(5);
    print "not "
        if $res != 120;
    say "ok 5 - __SUB__ # $res";
}

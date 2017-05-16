use v5;
use strict;
use feature 'say';

package Main;
say '1..3';

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
    say "ok 1 - __SUB__ # $res";
}

{
    use feature 'current_sub';

    # __SUB_ inside a do-BLOCK

    my $factorial = sub {
        my ($x) = @_;
        return 1 if $x == 1;
        do {
            return ( $x * __SUB__->( $x - 1 ) );
        }
    };

    my $res = $factorial->(5);
    print "not "
        if $res != 120;
    say "ok 2 - __SUB__ # $res";
}

{
    use feature 'current_sub';

    # __SUB_ inside a eval-BLOCK

    my $factorial = sub {
        my ($x) = @_;
        return 1 if $x == 1;
        eval {
            return ( $x * __SUB__->( $x - 1 ) );
        }
    };

    my $res = $factorial->(5);
    print "not "
        if $res != 120;
    say "ok 3 - __SUB__ # $res";
}

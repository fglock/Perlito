use v6;

class Main {
    say '1..2';
    my $v := 1 + 0.3;
    if ( $v < 1.29 ) || ( $v > 1.31 ) {
        print 'not '
    }
    say 'ok ', 1;

    if ( $v ~ '' ) ne '1.3' {
        print 'not '
    }
    say 'ok ', 2, ' # ', $v;
}

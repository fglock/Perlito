use v6;

class Main {
    say '1..1';
    my $v := 1 + 0.3;
    if ( $v < 1.29 ) || ( $v > 1.31 ) {
        print 'not '
    }
    say 'ok ', 1;
}

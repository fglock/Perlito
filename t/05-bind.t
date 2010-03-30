use v6;

class Main {

    say '1..2';

    my $x = 1;
    if $x != 1 {
        print 'not '
    };
    say 'ok ', $x;

    $x = 2;
    if $x != 2 {
        print 'not '
    };
    say 'ok ', $x;


}

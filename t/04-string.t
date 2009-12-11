use v6-alpha;

class Main {

    say '1..2';

    my $x := "abcd";
    if substr($x,1,1) ne "b" {
        print 'not '
    };
    say 'ok 1 - substr ', substr($x,1,1);

    if index($x,"c") ne 2 {
        print 'not '
    };
    say 'ok 2 - index ', index($x,"c");
}

use v5;

package Main;

    say '1..5';

    my $x = "abcd";
    if (substr($x,1,1) ne "b") {
        print 'not '
    };
    say 'ok 1 - substr ', substr($x,1,1);

    if (index($x,"c") ne 2) {
        print 'not '
    };
    say 'ok 2 - index ', index($x,"c");

    if (substr($x,3,1) ne "d") {
        print 'not '
    }
    say 'ok 3 - substr ', substr($x,3,1);

    print 'not ' if !defined "abc";
    say 'ok 4 - defined str';

    my $s = "o";
    $s .= "k 5 - concat";
    say $s;


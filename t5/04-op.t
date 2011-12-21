use v5;

package Main;

    say '1..11';

    my $x = 1;
    if ($x != "1") {
        print 'not '
    };
    say 'ok 1 - != ', $x;

    $x = 2;
    if (!($x eq "2")) {
        print 'not '
    };
    say 'ok 2 - ne ', $x;

    $x = 0 ? "not ok" : "ok";
    say $x, ' 3 - ternary';

    $x = 1 ? "ok" : "not ok";
    say $x, ' 4 - ternary';

    print "ok 5 - print with embedded newlines\nok 6 - more newlines\n";

    $x = 2;
    if (!(($x + 2) == ($x + 1 + 1))) {
        print 'not '
    };
    say 'ok 7 - add ';

    $x = 2;
    if (!(($x . 2) eq "22")) {
        print 'not '
    };
    say 'ok 8 - concat';

    my $undef;
    if $undef {
        print 'not '
    };
    say 'ok 9 - undef to bool';

    if (!(($undef . 2) eq "2")) {
        print 'not '
    };
    say 'ok 10 - undef to str';

    $x = 2;
    if (!(($x + 2) eq 4)) {
        print 'not '
    };
    say 'ok 11 - plus with string';


use v6;

class Main {
    say '1..3';
    my $a = sub () { 
            do { 5 }
        };
    if $a.() != 5 {
        print 'not '
    }
    say 'ok 1 - do inside function';

    $a = sub () { 
            return do { 5 };
            4;
        };
    if $a.() != 5 {
        print 'not '
    }
    say 'ok 2 - do inside function';

    $a = sub () { 
            do { return 5 };
            4;
        };
    if $a.() != 5 {
        print 'not '
    }
    say 'ok 3 - do inside function';
}

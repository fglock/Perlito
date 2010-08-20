use v6;

class Main {
    say '1..3';
    my %a;
    say 'ok 1 - create hash';
    %a{'abc'} = 3;
    say 'ok 2 - set element';
    if %a{'abc'} != 3 {
        print 'not '
    }
    say 'ok 3 - fetch element # ', %a{'abc'};

    %a{123} = 456;
    say '# values: ', %a.values;
    say '# keys:   ', %a.keys;
}

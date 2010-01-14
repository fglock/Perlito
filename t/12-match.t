use v6;

class Main {
    say '1..2';
    my $m := ::MiniPerl6::Match( str => 'abcdef', from => 2, to => 4, bool => 1 );
    say '# match scalar: ', $$m;
    if ($$m) eq 'cd' {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $<abc> := 3;
    say '# value is [', $<abc>,']';
    if ($<abc>) == 3 {
        say 'ok 2';
    }
    else {
        say 'not ok 2';
    }
}

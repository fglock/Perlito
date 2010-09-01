use v6;

class Main {
    say '1..2';
    my $m = Perlito::Match.new( str => 'abcdef', from => 2, to => 4, bool => 1 );
    say '# match scalar: ', $$m;
    if ($$m) eq 'cd' {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m{"abc"} = 3;
    say '# value is [', $m{"abc"},']';
    if ($m{"abc"}) == 3 {
        say 'ok 2';
    }
    else {
        say 'not ok 2';
    }
}

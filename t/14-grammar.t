use v6;

class Main {

    token digits {
        [ '1' | '2' | '3' ]+
    }

    say '1..1';
    my $m = Main.digits( '123', 0);
    say '# from: ', $m.from;
    say '# to:   ', $m.to;
    say 'ok 1';
}


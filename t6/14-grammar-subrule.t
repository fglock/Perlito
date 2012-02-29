use v6;

class Main {

    token digits {
        [ '1' | '2' | '3' ]+
    }
    token alfas {
        [ 'a' | 'b' | 'c' ]+
    }
    token alnums {
        [ <.digits> | <.alfas> ]+
    }

    say '1..1';
    my $m = Main.alnums( '123abc', 0);
    say '# from: ', $m.from;
    say '# to:   ', $m.to;
    if $m.to != 6 {
        print "not ";
    }
    say 'ok 1';
}


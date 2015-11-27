use v6;

class Main {

    token digit {
        [ '1' | '2' | '3' ] 
        { make "got a digit" }
    }
    token digits {
        <digit>*
        { 
            say '# before hyper ', $/.perl;
            make $<digit>.>>capture 
        }
    }

    say '1..1';

    {
        my $m = Main.digits( '123abc', 0);
        say '# from: ', $m.from;
        say '# to:   ', $m.to;
        say '# match: ', $m.perl;
        say '# capture: ', ($$m).perl;
        if $m.to != 3 {
            print "not ";
        }
        say 'ok 1';
    }

    {
        my $m = Main.digits( 'abc', 0);
        say '# from: ', $m.from;
        say '# to:   ', $m.to;
        say '# match: ', $m.perl;
        say '# capture: ', ($$m).perl;
        if $m.to != 0 {
            print "not ";
        }
        say 'ok 2';
    }

}


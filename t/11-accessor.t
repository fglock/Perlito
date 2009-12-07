use v6-alpha;

class Other {
    has $.a;
    method subr() { say 'ok ', $.a };
}

class Main {
    
    say '1..3';
    say 'ok 1 - load ok';

    my $other := ::Other( a => 2 );
    $other.subr();

    $other.a := 3;
    say 'ok ', $other.a;
}

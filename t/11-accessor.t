use v6;

class Other {
    has $.a;
    has @.x;
    method subr() { say 'ok ', $.a };
}

class Main {
    
    say '1..5';
    say 'ok 1 - load ok';

    my $other = Other.new( a => 2 );
    $other.subr();

    $other.a = 3;
    say 'ok ', $other.a;

    say 'ok 4' if $other.x.elems == 0;
    $other.x.push(10);
    say 'ok 5' if $other.x.elems == 1;
}

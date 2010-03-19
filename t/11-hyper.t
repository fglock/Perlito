use v6;

class Other {
    has $.a;
    method subr() { 
        say 'ok ', $.a;
        return $.a 
    };
}

class Main {
    
    say '1..3';
    say 'ok 1 - load ok';

    my $other := [ Other.new( a => 2 ), Other.new( a => 3 ) ];

    my @r := $other.>>subr();
    say '# [', @r, ']';
}

use strict;

print "1..2\n";

{
    my $foo = [ qw/x y z/ ];

    my $res = join(":", @{ $foo // [] } ) ; 

    print "not " unless $res eq 'x:y:z';
    print "ok 1\n";
}

{
    my $foo;

    my $res = join(":", @{ $foo // [ qw/ a b c /] } ) ; 

    print "not " unless $res eq 'a:b:c';
    print "ok 2\n";
}



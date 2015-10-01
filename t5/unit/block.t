use feature 'say';

say '1..1';

sub test {
    {
        456
    }
}

if ( test() != 456 ) {
    print "not ";
}
say 'ok 1 - true';	


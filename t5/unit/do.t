use feature 'say';

say '1..1';

sub test {
    do {
        456
    }
}

if ( test() != 456 ) {
    print "not ";
}
say 'ok 1 - true';	


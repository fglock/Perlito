use feature 'say';

say '1..2';

my $v;

sub test {
    if ($v) {
        123
    }
    else {
        456
    }
}

$v = 1;
if ( test() != 123 ) {
    print "not ";
}
say 'ok 1 - true';	

$v = 0;
if ( test() != 456 ) {
    print "not ";
}
say 'ok 2 - true';	


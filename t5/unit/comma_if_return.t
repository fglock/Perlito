use feature 'say';

say '1..2';

my $v;

sub test {
    if ($v) {
        123, 456
    }
    else {
        78, 90
    }
}

$v = 1;
if ( test() != 456 ) {
    print "not ";
}
say 'ok 1 - scalar + if + comma';	

$v = 0;
if ( join(":", test()) ne "78:90" ) {
    print "not ";
}
say 'ok 2 - list + if + comma ', join(":", test());	


use feature 'say';

say '1..3';

my $v;

sub test {
    for ($v) {
        $v++;
    }
}

$v = 1;
my @res = test();
if ( @res != 1 ) {
    print "not ";
}
say 'ok 1 - list # ', scalar(@res);	

if ( !defined $res[0] ) {
    print "not ";
}
say 'ok 2 - list # ', $res[0];	


my $res = test();
if ( !defined $res ) {
    print "not ";
}
say 'ok 3 - scalar # ', $res;	



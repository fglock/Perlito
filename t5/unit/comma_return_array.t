use feature 'say';

say '1..3';

my $v;

my @f = ( 1 .. 7 );
my @g = ( 1 .. 9 );

sub test {
    @f, @g
}

$v = 1;
if ( test() != 9 ) {
    print "not ";
}
say 'ok 1 - comma scalar context # ' . scalar test();	

$v = 0;
if ( @{[ test() ]} ne "16" ) {
    print "not ";
}
say 'ok 2 - comma list context # ' . @{[ test() ]};	

$v = 0;
if ( join(" ", test()) ne "1 2 3 4 5 6 7 1 2 3 4 5 6 7 8 9" ) {
    print "not ";
}
say 'ok 3 - comma list context # ' . join(" ", test());	


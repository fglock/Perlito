use feature 'say';

say '1..2';

my $v;

sub test {
    my @a = ( 789, 101112 );
    987, 123, 456, @a
}

$v = 1;
# in scalar context: return "number of elements in @a"
if ( test() != 2 ) {
    print "not ";
}
say 'ok 1 - comma scalar context [', scalar(test()), ']';	

$v = 0;
if ( join(" ", test()) ne "987 123 456 789 101112" ) {
    print "not ";
}
say "ok 2 - comma list context [@{[ test() ]}]";	


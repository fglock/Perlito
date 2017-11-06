use feature 'say';
# use Data::Dumper;

say '1..2';

my $v;

sub arg1 {
    # print Dumper( wantarray );
    print "not " if defined wantarray;
    say "ok 1";
}
sub arg2 {
    # print Dumper( wantarray );
    print "not " if !defined wantarray || wantarray ne '';
    say "ok 2";
}

scalar( arg1, arg2 );


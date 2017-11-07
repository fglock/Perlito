use feature 'say';
# use Data::Dumper;

say '1..4';

my $v;

sub arg_void {
    # print Dumper( wantarray );
    print "not " if defined wantarray;
    say "ok $_[0]";
}
sub arg_scalar {
    # print Dumper( wantarray );
    print "not " if !defined wantarray || wantarray ne '';
    say "ok $_[0]";
}

scalar( arg_scalar(1) );
scalar( arg_void(2), arg_void(3), arg_scalar(4) );


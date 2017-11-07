use feature 'say';
# use Data::Dumper;

say '1..8';

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
sub arg_list {
    # print Dumper( wantarray );
    print "not " if wantarray ne '1';
    say "ok $_[0]";
}
sub arg_error {
    # print Dumper( wantarray );
    print "not ";
    say "ok $_[0]";
}

my $v;
my @a;

$v = arg_scalar(1) ? arg_scalar(2) : arg_error(2);
$v = !arg_scalar(3) ? arg_error(4) : arg_scalar(4);

@a = arg_scalar(5) ? arg_list(6) : arg_error(6);
@a = !arg_scalar(7) ? arg_error(8) : arg_list(8);


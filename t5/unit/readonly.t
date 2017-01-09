use feature 'say';

say '1..5';
my @a;

@a = ( 1, 2 );
for my $v (@a) {
    $v++;
}
print "not " if $a[0] != 2 || $a[1] != 3;
say "ok 1 - loop variable is rw alias";

eval {
    @a = ( 1, 2 );
    for my $v ( @a, 3, 4 ) {
        $v++;
    }
    1;
}
  and print "not ";
say "ok 2 - loop: Modification of a read-only value attempted: $@";

sub change { $_[0]++ }

@a = (1, 2);
change(@a);
print "not " if $a[0] != 2;
say "ok 3 - parameter is rw alias";

change($a[1]);
print "not " if $a[1] != 3;
say "ok 4 - parameter is rw alias";

eval {
    change(3);
    1;
}
  and print "not ";
say "ok 5 - parameter: Modification of a read-only value attempted: $@";


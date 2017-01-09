use feature 'say';

say '1..2';
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
say "ok 2 - Modification of a read-only value attempted: $@";


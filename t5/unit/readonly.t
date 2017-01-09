use feature 'say';

say '1..8';
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
sub change1 { $_[1]++ }

@a = (1, 2);
change(@a);
print "not " if $a[0] != 2;
say "ok 3 - parameter is rw alias";

change($a[1]);
print "not " if $a[1] != 3;
say "ok 4 - parameter is rw alias";

my %h = ( aa => 123 );
change(%h);
print "not " if $h{aa} != 123;
say "ok 5 - parameter hash key is copy";
change1(%h);
print "not " if $h{aa} != 124;
say "ok 6 - parameter hash value is alias";

eval {
    change(4..6);
    1;
}
  or print "not ";
say "ok 7 - parameter range is copy";

eval {
    change(3);
    1;
}
  and print "not ";
say "ok 8 - parameter: Modification of a read-only value attempted: $@";


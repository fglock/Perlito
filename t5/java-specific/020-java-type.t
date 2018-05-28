
use v5;
use feature 'say';
use Java;

say "1..1";

sub UUID () { Java->type("java.util.UUID") }

my $s = UUID->randomUUID()->toString();

if (length($s) < 10) {
    print "not ";
}
say "ok 1 # $s";    # 9e8aadb3-4d81-41c6-af85-7ab7213a9945

say "# done";


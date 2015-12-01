use v5;
use strict;
use feature 'say';

say '1..3';
my $test = 1;
my @v;

sub is_res {
    my ($expect) = @_;
    my $res = "@v";
    print "not " if $expect ne $res;
    say "ok $test # expect $expect, got [$res]";
    $test++;
}

@v = scalar reverse "abc";
is_res "cba";

@v = reverse "abc";
is_res "abc";

@v = ();
for my $i (reverse "abc") {
    push @v, $i;
}
is_res "abc";


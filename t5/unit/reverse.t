use v5;
use strict;
use feature 'say';

say '1..7';
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

@v = reverse 4, 5, 6;
is_res "6 5 4";

@v = (reverse 4, 5, 6);
is_res "6 5 4";

@v = reverse (4, 5, 6);
is_res "6 5 4";

@v = scalar reverse (4, 5, 6);
is_res "654";


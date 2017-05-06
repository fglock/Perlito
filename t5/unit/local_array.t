use v5;
use strict;
use feature 'say';

say '1..7';

@X::v = (10, 15, 20, 25);
my $str = "10 15 20 25";

my $vv = \@X::v;

if (1) {
    print "not " if "@X::v" ne $str;
    say "ok 1";

    local @X::v;
    print "not " if scalar @X::v;
    say "ok 2";

    print "not " if "@$vv" ne $str;
    say "ok 3 # @$vv";

    @X::v = (15);
    print "not " if "@X::v" ne "15";
    say "ok 4";

    print "not " if "@$vv" ne $str;
    say "ok 5 # @$vv";

}

print "not " if "@X::v" ne $str;
say "ok 6 - exit scope retrieves old value";

print "not " if "@$vv" ne $str;
say "ok 7";


use v5;
use strict;
use feature 'say';

say '1..7';

%X::v = (10, 15, 20, 25);
my $str1 = "10 15 20 25";
my $str2 = "20 25 10 15";

my $vv = \%X::v;

if (1) {
    print "not " if "@{[ %X::v ]}" ne $str1 && "@{[ %X::v ]}" ne $str2;
    say "ok 1";

    local %X::v;
    print "not " if keys %X::v;
    say "ok 2";

    print "not " if "@{[ %$vv ]}" ne $str1 && "@{[ %$vv ]}" ne $str2;
    say "ok 3";

    %X::v = (15, 20);
    print "not " if "@{[ %X::v ]}" ne "15 20";
    say "ok 4 # @{[ %X::v ]}";

    print "not " if "@{[ %$vv ]}" ne $str1 && "@{[ %$vv ]}" ne $str2;
    say "ok 5";

}

print "not " if "@{[ %X::v ]}" ne $str1 && "@{[ %$vv ]}" ne $str2;
say "ok 6 - exit scope retrieves old value";

print "not " if "@{[ %$vv ]}" ne $str1 && "@{[ %$vv ]}" ne $str2;
say "ok 7";


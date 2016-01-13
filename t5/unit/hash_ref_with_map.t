#!./perl -w

BEGIN {
    chdir 't' if -d 't';
    @INC = '../lib';
    require './test.pl';
}

plan( tests => 2 );

my $foo = [5,200];
my $x = { (map { $_, ($_*2) } @$foo)};

is ($x->{5} , 10, "map+hashref: x->5");
is ($x->{200}, 400, "map+hashref: x->200");

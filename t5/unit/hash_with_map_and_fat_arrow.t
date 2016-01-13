#!./perl -w

BEGIN {
    chdir 't' if -d 't';
    @INC = '../lib';
    require './test.pl';
}

plan( tests => 3 );

my %x = (map { $_ => 1 } qw(one two three));

is ($x{'one'} , 1, "map+fatarrow x{one}");
is ($x{'three'}, 1, "map+fatarrow: x{three}");
ok (!exists($x{'four'}), "x{four} does not exist");

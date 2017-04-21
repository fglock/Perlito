#!./perl

no strict; # preload strict.pm

BEGIN {
    chdir 't' if -d 't';
    @INC = '../lib';
    require './test.pl';
}

use strict;
plan( tests => 21 );


sub run_tests {

# Tests for NUL characters.
{
    my @tests = (
        ["",            -1, -1, -1],
        ["foo",         -1, -1, -1],
        ["\0",           0, -1, -1],
        ["\0\0",         0,  0, -1],
        ["\0\0\0",       0,  0,  0],
        ["foo\0",        3, -1, -1],
        ["foo\0foo\0\0", 3,  7, -1],
    );
    foreach my $l (1 .. 3) {
        my $q = "\0" x $l;
        my $i = 0;
        foreach my $test (@tests) {
            $i ++;
            my $str = $$test [0];
            my $res = $$test [$l];

            #
            # Bug #53746 shows a difference between variables and literals,
            # so test literals as well.
            #
            my $test_str = qq {is (index ("$str", "$q"), $res, } .
                           qq {"Find NUL character(s)")};
               $test_str =~ s/\0/\\0/g;

            eval $test_str;
            die $@ if $@;
        }
    }
}

}

run_tests() unless 0;

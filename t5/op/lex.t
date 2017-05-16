#!perl
use strict;
use warnings;

BEGIN {
    no strict;
    chdir 't' if -d 't';
    @INC = '../lib';
    require './test.pl';
}

plan(tests => 2);



{
    my %foo = (aap => "monkey");
    my $foo = '';
    is("@{[$foo{'aap'}]}", 'monkey', 'interpolation of hash lookup with space between lexical variable and subscript');
    is("@{[$foo {'aap'}]}", 'monkey', 'interpolation of hash lookup with space between lexical variable and subscript - test for [perl #70091]');


}


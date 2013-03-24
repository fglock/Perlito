#
# this test can be run with:
#
#   perl -Isrc5/lib t5-x64/01_sanity.t
#
#   node perlito5.js -Isrc5/lib t5-x64/01_sanity.t
#


use strict;
use warnings;
use feature 'say';
use Perlito5::X64::Assembler;

say "1..1";

my $asm = Perlito5::X64::Assembler->new();

{
    package Perlito5::X64::Assembler;

    ret();
    my $out = to_hex();

    print "not " if $out ne 'C3';
    say "ok # $out";
}


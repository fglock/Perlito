use strict;
use warnings;
use Perlito5::X64::Assembler;
use Perlito5::Test;

say "1..1";

my $asm = Perlito5::X64::Assembler->new();

{
    package Perlito5::X64::Assembler;

    ret();
    my $out = to_hex();

    print "not " if $out ne 'C3';
    say "ok # $out";
}


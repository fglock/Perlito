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

say "1..4";

{
    package Perlito5::X64::Assembler;
    my $out;

    $out = is_register( rax );
        print "not " if !$out;
        say "ok # is_register";
    $out = is_register( 0x0A );
        print "not " if $out;
        say "ok # !is_register";

    asm_reset();
    _ret();
        $out = to_hex();
        print "not " if $out ne 'C3';
        say "ok # $out";
    _ret(10);
        $out = to_hex();
        print "not " if $out ne 'C3 C2 0A 00';
        say "ok # $out";

    asm_reset();
    _push( rax );
    _push( r14 );
    _push( 120 );
    # TODO - test negative numbers
    say "# push " . to_hex();

    asm_reset();
    _pop( rax );
    _pop( r14 );
    say "# pop " . to_hex();

    asm_reset();
    _movq( rax, rcx );
    say "# movq " . to_hex();

    asm_reset();
    _shld( rax, rcx );
    say "# shld " . to_hex();

    asm_reset();
    my $here = label;
    _xchg( rax, rcx );
    _bind($here);
    _neg( rbx );
    _jmp($here);
    _j(equal, $here);
    _jmp( rcx );
    say "# xchg " . to_hex();
    say "# label pos=", $here->pos();


    # "Hello, world"
    asm_reset();
    # set the data pointers to something
    my $message = 0x1000; # point to the string
    my $length  = 0;
    # sys_write(stdout, message, length)
    _movq rax, 1        ; # sys_write
    _movq rdi, 1        ; # stdout
    _movq rsi, $message ; # message address
    _movq rdx, $length  ; # message string length
    _syscall           ;
    # sys_exit(return_code)
    _movq rax, 60       ; # sys_exit
    _movq rdi, 0        ; # return 0 (success)
    _syscall            ;
    say "# hello world: " . to_hex();


}


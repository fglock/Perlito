use feature 'say';
use strict;
use warnings;

say '1..4';

{
no warnings;    # Illegal octal digit '8' ignored at t/oct_op.t line 7.
my $x = oct "8";
if ($x != 0) {
    print 'not ';
}
say 'ok 1 - 0, got ', $x;
}

my $x = oct "0xa";
if ($x != 10) {
    print 'not ';
}
say 'ok 2 - wanted 10, got ', $x;

$x = oct "0XA";
if ($x != 10) {
    print 'not ';
}
say 'ok 3 - wanted 10, got ', $x;

$x = oct "hello";
if ($x != 0) {
    print 'not ';
}
say 'ok 4 - wanted 0, got ', $x;

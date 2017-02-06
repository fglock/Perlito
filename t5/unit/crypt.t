use feature 'say';
use strict;
use warnings;

say '1..2';

my $x = crypt "hello", "ab";
if ($x ne "abl0JrMf6tlhw") {
    print 'not ';
}
say 'ok 1 - wanted abl0JrMf6tlhw, got ', $x;

$x = crypt "Perlito", "longer as needed";
if ($x ne "loQTxEbOR1T6U") {
    print 'not ';
}
say 'ok 2 - wanted loQTxEbOR1T6U, got ', $x;

# system-dependent - an invalid salt may return undef
# $x = crypt "", "";
# if ($x ne "..X8NBuQ4l6uQ") {
#     print 'not ';
# }
# say 'ok 3 - wanted ..X8NBuQ4l6uQ, got ', $x;

# TODO Perl does compile time, not possible yet
# $ perl t5/unit/crypt.t 
# Too many arguments for crypt at t5/unit/crypt.t line 26, near ""arguments";"
# Execution of t5/unit/crypt.t aborted due to compilation errors.
# eval {
#     $x = crypt "With", "three", "arguments";
#     print 'not ';
# };
# say 'ok 4 - wanted to die, died: ', $@;



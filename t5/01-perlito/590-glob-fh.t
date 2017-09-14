use strict;
use warnings;
use feature 'say';

say "1..2";

my $fh = *STDOUT;
$fh->print("ok 1 - print to *STDOUT fh works.\n");

my $gh = \*STDOUT;
$gh->print("ok 2 - print to \\*STDOUT fh works.\n");


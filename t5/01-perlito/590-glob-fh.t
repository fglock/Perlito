use strict;
use warnings;
use feature 'say';

say "1..1\n";
my $fh = *STDOUT;
$fh->print("ok 1 - print to *STDOUT fh works.\n");

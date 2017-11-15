#! /usr/bin/perl

use strict;
use Data::Dumper;

my $test_report = shift || die "Usage: makefiles/copy-tests.pl t5/Test-summary-report-5jar.txt";

open my $f, "<", $test_report;

my @files = <$f>;

# warn Dumper(\@files);

for my $line (@files) {
    if ($line =~ m{^t5/(\S+)}) {
        my $bad_file = 't/' . $1;
        # warn "$bad_file\n";
        unlink $bad_file;
    }
}


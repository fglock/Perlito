#! /usr/bin/perl

use strict;
use Data::Dumper;

my $usage = "Usage: makefiles/copy-tests.pl t5/Test-summary-report-5jar.txt t5-jar";

my $test_report = shift || die $usage;
my $out_dir = shift || die $usage;

open my $f, "<", $test_report;

my @files = <$f>;

# warn Dumper(\@files);
warn "running from directory: ", system("pwd");

for my $line (@files) {
    if ($line =~ m{^t5/(\S+)}) {
        my $bad_file = $out_dir . '/' . $1;
        # warn "$bad_file\n";
        unlink $bad_file
            or (system("rm", $bad_file) == 0)
            or warn "Can't unlink $bad_file: $!";
    }
}


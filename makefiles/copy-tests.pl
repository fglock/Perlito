#! /usr/bin/perl

use strict;
use Data::Dumper;
use File::Find;
use File::Copy;

my $usage = "Usage: makefiles/copy-tests.pl t5/Test-summary-report-5jar.txt t5-jar";

my $test_report = shift || die $usage;
my $out_dir     = shift || die $usage;

open my $f, "<", $test_report;

my @files = <$f>;

# warn "running from directory: ", `pwd`;

my %bad_file;
for my $line (@files) {
    if ( $line =~ m{^(t5/\S+)} ) {
        $bad_file{$1} = 1;

        # warn "bad file $1\n";
    }
}

my @files;
my @dir;
finddepth(
    {
        no_chdir => 1,
        wanted   => sub {
            # warn "name $File::Find::name ", ( -d $File::Find::name ? "dir" : "not" ), "\n";
            if   ( -d $File::Find::name ) { push @dir,   $File::Find::name }
            else                          { push @files, $File::Find::name }
        },
    },
    "t5"
);

for my $name ( sort { length($a) <=> length($b) } @dir) {
    # warn "$name is directory\n";
    $name =~ s/^t5/$out_dir/;
    if (! -d $name) {
        mkdir $name
            or warn "Can't mkdir $name\n";
    }
}
for my $name (@files) {
    # warn "file $name\n";
    if ( $bad_file{$name} ) {
        # warn "bad file $name\n";
        next;
    }
    my $out = $name;
    $out =~ s/^t5/$out_dir/;
    # warn "copy file $name to $out\n";
    copy( $name, $out )
      or warn "Copy $name to $out failed: $!";
}


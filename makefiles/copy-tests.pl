#! /usr/bin/perl

use strict;
use Data::Dumper;
use File::Find;
use File::Copy;

my $usage = "Usage: makefiles/copy-tests.pl t5/Test-summary-report-5jar.txt t5-jar";

my $test_report = shift || die $usage;
my $out_dir     = shift || die $usage;

# warn "running from directory: ", `pwd`;

# Read the "test_report" with the list of tests that cannot run in this platform
open my $f, "<", $test_report;
my @files = <$f>;
my %bad_file;
my @bad_directory;
for my $line (@files) {
    if ( $line =~ m{^(t5/\S+)} ) {
        my $name = $1;
        $bad_file{$name} = 1;
        push @bad_directory, $name if $name =~ m{/$};
        # warn "bad file $name\n";
    }
}

# Collect all test filenames from "t5/"
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

# Create the destination directories
for my $name ( sort { length($a) <=> length($b) } @dir) {
    # warn "$name is directory\n";
    if ( grep { "$name/" =~ /^$_/ } @bad_directory ) {
        # warn "bad directory $name\n";
        next;
    }
    $name =~ s/^t5/$out_dir/;
    if (! -d $name) {
        mkdir $name
            or warn "Can't mkdir $name\n";
    }
}
# Copy the test files
for my $name (@files) {
    # warn "file $name\n";
    if ( $bad_file{$name} || grep { $name =~ /^$_/ } @bad_directory ) {
        # warn "bad file $name\n";
        next;
    }
    my $out = $name;
    $out =~ s/^t5/$out_dir/;
    # warn "copy file $name to $out\n";
    copy( $name, $out )
      or warn "Copy $name to $out failed: $!";
}

# some Perl tests need "t/TEST" and "t/test.pl"
mkdir "t";
copy "t5/TEST", "t/TEST";
copy "t5/test.pl", "t/test.pl";

# some Perl tests have a hardcoded "lib" directory
mkdir "lib";
mkdir "lib/warnings";
for my $file ( "Config.pm", "overload.pm", "overloading.pm", "warnings/register.pm" ) {
    copy "src5/lib/Perlito5X/$file", "lib/$file";
}


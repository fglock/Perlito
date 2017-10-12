
use strict;
use Data::Dumper;
use Cwd;

my $dir = getcwd;
my $test_dir = "./t5";

sub open_dir {
    my $test_dir = shift;
    opendir( my $dh, $test_dir ) or die "can't open dir $test_dir: $!";
    my @files = readdir($dh);
    closedir $dh;
    my @out;
    for my $test_file (@files) {
        my $file_name = "$test_dir/$test_file";
        if ( -d $file_name && substr($test_file, 0, 1) ne "." ) {
            push @out, open_dir($file_name);
        }
        elsif ( -f $file_name && substr($file_name, -2, 2) eq ".t" ) {
            push @out, $file_name;
        }
    }
    return @out;
}

my @files = sort { $a cmp $b } open_dir($test_dir);
print Dumper \@files;

for my $test_file (@files) {
    print "$test_file ...\n";

    chdir $dir;
    local @INC;
    do $test_file;

}


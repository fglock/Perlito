#!/usr/bin/perl
# recompiles MiniPerl6 using itself saving result in lib*-new
use strict;
use warnings;

my $target_switch = shift || '-Cperl5';
my $backend;
my $target_dir;
my $target_suffix;
if ( $target_switch eq '-Cperl5' ) {
    $backend    = 'perl5';    
    $target_dir = 'lib5';
    $target_suffix = '.pm';
}
elsif ( $target_switch eq '-Clisp' ) {
    $backend    = 'lisp';    
    $target_dir = 'liblisp';
    $target_suffix = '.lisp';
}
elsif ( $target_switch eq '-Cjs' ) {
    $backend    = 'javascript';    
    $target_dir = 'libjs';
    $target_suffix = '.js';
}
else {
    die "invalid option: $target_switch\n";
}

system( "rm -r '${target_dir}-new'" );
make( 'lib', $target_dir, "${target_dir}-new" );

if ( test( "${target_dir}-new" ) ) {
    backup( $target_dir );
    rename( $target_dir, "${target_dir}-new" );
}

#---

sub test {
    warn "automatic testing is not implemented yet\n";
    return 0;
}
sub backup {
}
sub compile {
    my ($in,$out) = @_;
    print("perl mp6.pl $target_switch $in > $out\n");
    system("perl mp6.pl $target_switch $in > $out");
}
sub make {
    my ($source,$old,$new) = @_;
    mkdir($new);
    my %seen;
    for my $dir (`find $source -type d`) {
        chomp($dir);
        $dir =~ s/^\Q$source\/\E//;
        print("mkdir $new/$dir\n");
        mkdir("$new/$dir");
    }
    for my $file (`find $source -name '*.pm'`) {
        chomp($file);
        $file =~ s/^\Q$source\/\E//;

        next if $file eq "MiniPerl6/Perl5/Match.pm";     # skip - this is a perl5 file 
        next if $file eq "MiniPerl6/Perl5/Runtime.pm";   # skip - this is a perl5 file 

        my $new_file = $file;
        $new_file =~ s/\.pm$/$target_suffix/;

        compile("$source/$file","$new/$new_file");
        $seen{$file} = 1;
    }
    for my $file (`find $source -name '*.*'`) {
        chomp($file);
        $file =~ s/^\Q$source\/\E//;
        if ( !$seen{$file} ) {
            print("cp '$source/$file' '$new/$file'\n");
            system("cp '$source/$file' '$new/$file'");
        }
    }
}


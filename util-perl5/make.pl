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
elsif ( $target_switch eq '-Cparrot' ) {
    $backend    = 'parrot';    
    $target_dir = 'libparrot';
    $target_suffix = '.pir';
}
elsif ( $target_switch eq '-Cpython' ) {
    $backend    = 'python';    
    $target_dir = 'libpy';
    $target_suffix = '.py';
}
elsif ( $target_switch eq '-Cast-perl5' ) {
    $backend    = 'ast-perl5';    
    $target_dir = 'libast-perl5';
    $target_suffix = '.p5ast';
}
elsif ( $target_switch eq '-Cast-json' ) {
    $backend    = 'ast-json';    
    $target_dir = 'libast-json';
    $target_suffix = '.json';
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
    warn "\n";
    warn "automatic testing is not implemented yet. Please use one of:\n";
    for ( qw( perl5 lisp js go python ) ) {
        warn "  prove -e \"perl mp6.pl -B$_\"\n"
    }
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
        next if $dir eq 'lib';
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

        my $ast_file = $file;
        $ast_file =~ s/\.pm$/.p5ast/;
        $ast_file = "libast-perl5/$ast_file";
        my $date_ast = -M $ast_file;
        my $date_src = -M "$source/$file";
        # warn " src $date_src ast $ast_file $date_ast ", ( $date_ast && $date_ast < $date_src && -s $ast_file ? "ok" : "not ok" ), "\n";
        if ( $date_ast && $date_ast < $date_src && -s $ast_file ) {
            # warn "ast looks ok\n";
            if ( $backend eq 'ast-perl5' ) {
                # skip
                print("cp '$ast_file' '$new/$new_file'\n");
                system("cp '$ast_file' '$new/$new_file'");
            }
            else {
                compile( $ast_file, "$new/$new_file" );
            }
        }
        else {
            # warn "ast is outdated\n";
            compile("$source/$file","$new/$new_file");
        }
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


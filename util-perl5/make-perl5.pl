#!/usr/bin/perl
# recompiles MiniPerl6 using itself saving resault in lib5-new
use strict;
use warnings;
sub test {
    warn "automatic testing is not implemented yet\n";
    return 0;
}
sub backup {
}
sub compile {
    my ($in,$out) = @_;
    print("perl mp6.pl -Cperl5 $in > $out\n");
    system("perl mp6.pl -Cperl5 $in > $out");
}
sub make {
    my ($source,$old,$new) = @_;
    mkdir($new);
    for my $dir (`find $source -type d`) {
        chomp($dir);
        $dir =~ s/^\Q$source\/\E//;
        print("mkdir $new/$dir\n");
        mkdir("$new/$dir");
    }
    for my $file (`find $source -name '*.pm'`) {
        chomp($file);
        $file =~ s/^\Q$source\/\E//;
        #print("compile $new/$file\n");
        compile("$source/$file","$new/$file");
    }
}

make('lib','lib5','lib5-new');

if (test 'lib5-new') {
    backup('lib5');
    rename('lib5','lib5-new');
}

# $ perl -I src5/lib misc/Perl5/core_redefine.pl

use strict;
use Perlito5::Java::Apply;
use Data::Dumper;

my $ops = Perlito5::AST::Apply::emit_java_op_table();

$ops->{$_} = 1
    for qw/
        __END__
        __LINE__
        __FILE__
        __DATA__
        require
        chdir
        chroot
        chmod
        chown
    /;
# print "[ @{[ sort keys %$ops ]} ]\n";

for my $op ( sort keys %$ops ) {
    next if $op =~ /</;

    local $@;

    my $prototype = eval { prototype "CORE::$op" };
    if (defined $prototype) {
        # print "\t$op $prototype\n";
    }

    my $v = int(rand(10000));
    my $x =
        eval "BEGIN { *CORE::GLOBAL::$op = sub { \$v } } "
           . "$op()";
    # print $@ if $@;
    if ($v eq $x) {
        print "$op\n";
    }
    else {

        $x =
            eval "BEGIN { *CORE::GLOBAL::$op = sub { \$v } } "
               . "$op('test')";
        # print $@ if $@;
        if ($v eq $x) {
            print "$op\n";
        }

    }
}


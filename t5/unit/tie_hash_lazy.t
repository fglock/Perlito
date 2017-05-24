
# use Data::Dumper;
use strict;

package TheHash;

my $debug = 0;

sub TIEHASH {
    my $storage = bless {}, shift;
    warn "New ReportHash created, stored in $storage.\n" if $debug;
    $storage;
}

sub STORE {
    warn "STORE data with key $_[1] at $_[0].\n" if $debug;
    $_[0]{ $_[1] } = $_[2];
}

sub FETCH {
    warn "FETCH data with key $_[1] at $_[0].\n" if $debug;
    $_[0]{ $_[1] };
}

sub FIRSTKEY {
    warn "FIRSTKEY\n" if $debug;
    each %{$_[0]};
}

sub NEXTKEY {
    warn "NEXTKEY\n" if $debug;
    each %{$_[0]};
}

# sub dump {
#     print STDERR Data::Dumper::Dumper($_[0]);
# }

package main;

print "1..2\n";

my $tied = tie( my %hh, 'TheHash' );

sub addr {
    $_++ for @_;
}

addr( $hh{x} );

print "not " if $hh{x} != 1;
print "ok 1\n";

addr( %hh );

print "not " if $hh{x} != 2;
print "ok 2\n";

# $tied->dump();


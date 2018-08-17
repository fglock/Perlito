package Perlito5::Runtime::Sprintf;
use strict;
use warnings;

# a pure Perl implementation of some sprintf() helpers

sub sprintf_vd {
    my ($v) = shift;
    if (UNIVERSAL::isa($v, "version")) {
        $v = join( "", map { chr($_) } @{ $v->{version} } );
    }
    return join( ".", map { ord($_) } split //, $v );
}

1;


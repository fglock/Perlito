package Carp;

#       carp    - warn of errors (from perspective of caller)
sub carp {
    warn @_;
}

#       cluck   - warn of errors with stack backtrace
#                 (not exported by default)
sub cluck {
    warn @_;
}

#       croak   - die of errors (from perspective of caller)
sub croak {
    die @_;
}

#       confess - die of errors with stack backtrace
sub confess {
    die @_;
}

sub import {
    my $self = shift;
    my ($pkg) = caller();
    my @exports = @_;

    push @exports, 'carp'
        unless grep { $_ eq 'carp' } @exports;
    push @exports, 'croak'
        unless grep { $_ eq 'croak' } @exports;

    # print "called from $pkg [ @exports ]\n";

    for my $export (@exports) {
        *{ $pkg . '::' . $export } = \&{ $export };
    }

}

sub unimport {
}

1;


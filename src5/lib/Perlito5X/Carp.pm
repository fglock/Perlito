package Carp;

our (@ISA, @EXPORT, @EXPORT_OK, @EXPORT_FAIL);
BEGIN {
    @EXPORT    = qw(confess croak carp);
    @EXPORT_OK = qw(cluck verbose longmess shortmess);
    @EXPORT_FAIL = qw(verbose);    # hook to enable verbose mode
}
use Exporter qw(import);

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

1;


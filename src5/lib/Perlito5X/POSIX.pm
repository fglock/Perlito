package POSIX;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(
    floor
    ceil
    strftime
);

sub floor {
    # from: Math::Utils
    ($_[0] < 0 and int($_[0]) != $_[0])? int($_[0] - 1): int($_[0])
}

sub ceil {
    # from: Math::Utils
    ($_[0] > 0 and int($_[0]) != $_[0])? int($_[0] + 1): int($_[0])
}

sub strftime {
    return "strftime not yet implemented";  # TODO See: DateTime.pm
}

1;


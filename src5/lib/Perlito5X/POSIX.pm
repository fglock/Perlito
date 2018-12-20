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

my %fmt = (
    '%Y' => sub { return 1900 + $_[6] },
    '%m' => sub { return sprintf( "%02d", 1 + $_[5] ) },
    '%d' => sub { return sprintf( "%02d", 0 + $_[4] ) },
    '%H' => sub { return sprintf( "%02d", 0 + $_[3] ) },
    '%M' => sub { return sprintf( "%02d", 0 + $_[2] ) },
    '%S' => sub { return sprintf( "%02d", 0 + $_[1] ) },
);

sub strftime {
    #  strftime('%d %b %Y %H:%M', 0, $5, $4, $3, int($2)-1, int($1)-1900);
    #  strftime($format, 0, 0, 0, $3, int($2)-1, int($1)-1900);
    #  strftime("%Y-%m-%d %H:%M", localtime)

    if (exists %fmt{$_[0]}) {
        return $fmt{$_[0]}->(@_);
    }
    # my $format = $_[0];
    # if ($format eq '%Y') { return 1900 + $_[6] };
    # if ($format eq '%m') { return 1 + $_[5] };
    # if ($format eq '%d') { return 0 + $_[4] };

    return "strftime not yet implemented";  # TODO See: DateTime.pm
}

1;


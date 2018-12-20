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
    '%S' => sub { sprintf( "%02d", 0 + $_[1] ) },
    '%M' => sub { sprintf( "%02d", 0 + $_[2] ) },
    '%H' => sub { sprintf( "%02d", 0 + $_[3] ) },
    '%k' => sub { sprintf( "%2d",  0 + $_[3] ) },
    '%d' => sub { sprintf( "%02d", 0 + $_[4] ) },
    '%e' => sub { sprintf( "%2d",  0 + $_[4] ) },
    '%m' => sub { sprintf( "%02d", 1 + $_[5] ) },
    '%Y' => sub { 1900 + $_[6] },
    '%u' => sub { (0 + $_[7]) ? (0 + $_[7]) : 7 },
    '%w' => sub { 0 + $_[7] },
    '%C' => sub { sprintf( "%02d", (1900 + $_[6])/100 ) },
    '%n' => sub { "\n" },
    '%t' => sub { "\t" },
    '%%' => sub { "%" },
    '%D' => sub { strftime('%m/%d/%y', @_[1 .. 9]) },
    '%F' => sub { strftime('%Y-%m-%d', @_[1 .. 9]) },
    '%r' => sub { strftime('%I:%M:%S %p', @_[1 .. 9]) },
    '%R' => sub { strftime('%H:%M', @_[1 .. 9]) },
    '%T' => sub { strftime('%H:%M:%S', @_[1 .. 9]) },
);

sub strftime {
    #  strftime('%d %b %Y %H:%M', 0, $5, $4, $3, int($2)-1, int($1)-1900);
    #  strftime($format, 0, 0, 0, $3, int($2)-1, int($1)-1900);
    #  strftime("%Y-%m-%d %H:%M", localtime)

    if (exists $fmt{$_[0]}) {
        return $fmt{$_[0]}->(@_);
    }
    my @parts = split /(%\w)/, $_[0];
    return join("", map {
        !defined $_ ? ""
        : exists $fmt{$_} ? $fmt{$_}->($_, @_[1 .. 9])
        : $_
    } @parts);
}

1;


package Text::Unidecode;
use strict;

use Exporter qw(import);
our @EXPORT = qw(
    unidecode
);
our @EXPORT_OK = qw();

sub unidecode {
    return $_[0]
}

1;

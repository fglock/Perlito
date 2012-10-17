package strict;

sub import {
    $Perlito5::STRICT = 1;
}

sub unimport {
    $Perlito5::STRICT = 0;
}

1;


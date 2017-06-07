package strict;

sub import {
    $Perlito5::STRICT = 1;
    $Perlito5::HINT   = 2018;   # TODO - subs/refs/vars
}

sub unimport {
    $Perlito5::STRICT = 0;
    $Perlito5::HINT   = 0;      # TODO - subs/refs/vars
}

1;


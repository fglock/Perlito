package strict;

sub import {
    $Perlito5::STRICT = 1;
    $^H   = 2018;   # TODO - subs/refs/vars
}

sub unimport {
    $Perlito5::STRICT = 0;
    $^H   = 0;      # TODO - subs/refs/vars
}

1;


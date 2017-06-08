package strict;

sub import {
    $^H   = 2018;   # TODO - subs/refs/vars
}

sub unimport {
    $^H   = 0;      # TODO - subs/refs/vars
}

1;


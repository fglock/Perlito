package Perlito5::bytes;

sub import {
    $Perlito5::BYTES = 1;
}

sub unimport {
    $Perlito5::BYTES = 0;
}

1;


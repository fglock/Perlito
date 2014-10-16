package Perlito5::encoding;

sub import {
    $Perlito5::ENCODING = undef;
}

sub unimport {
    $Perlito5::ENCODING = undef;
}

1;


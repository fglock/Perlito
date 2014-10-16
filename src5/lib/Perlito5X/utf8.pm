package Perlito5::utf8;

sub import {
    $Perlito5::UTF8 = 1;
}

sub unimport {
    $Perlito5::UTF8 = 0;
}

1;


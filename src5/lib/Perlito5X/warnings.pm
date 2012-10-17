package warnings;

sub import {
    $Perlito5::WARNINGS = 1;
}

sub unimport {
    $Perlito5::WARNINGS = 0;
}

1;


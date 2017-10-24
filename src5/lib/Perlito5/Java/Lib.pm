package Perlito5::Java::Lib;

use Perlito5::Grammar::Use;
use strict;

sub init {
    @Perlito5::Grammar::Use::Perlito_internal_lib_directory = (
        'Perlito5X/Java',
        'Perlito5X',
        '',
    );
}

1;


package Perlito5::JavaScript2::Lib;
use strict;

sub init {
    Perlito5::Grammar::Use::register_internal_module('MIME::Base64' => 'Perlito5X::JavaScript::MIME::Base64');
}

1;


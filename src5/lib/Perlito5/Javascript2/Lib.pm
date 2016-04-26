package Perlito5::Javascript2::Lib;
use strict;

sub init {
    Perlito5::Grammar::Use::register_internal_module('MIME::Base64' => 'Perlito5X::Javascript::MIME::Base64');
}

1;


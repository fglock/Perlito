package Perlito5::Java::Lib;

use Perlito5::Grammar::Use;
use strict;

sub init {
    Perlito5::Grammar::Use::register_internal_module( 'MIME::Base64' => 'Perlito5X::Java::MIME::Base64' );
    Perlito5::Grammar::Use::register_internal_module( 'Scalar::Util' => 'Perlito5X::Java::Scalar::Util' );
}

1;


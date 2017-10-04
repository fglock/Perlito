package Perlito5::Java::Lib;

use Perlito5::Grammar::Use;
use strict;

sub init {
    Perlito5::Grammar::Use::register_internal_module( 'Cwd'          => 'Perlito5X::Java::Cwd' );
    Perlito5::Grammar::Use::register_internal_module( 'MIME::Base64' => 'Perlito5X::Java::MIME::Base64' );
    Perlito5::Grammar::Use::register_internal_module( 'Scalar::Util' => 'Perlito5X::Java::Scalar::Util' );
    Perlito5::Grammar::Use::register_internal_module( 'List::Util'   => 'Perlito5X::Java::List::Util' );
    Perlito5::Grammar::Use::register_internal_module( 'Digest::MD5'  => 'Perlito5X::Java::Digest::MD5' );
    Perlito5::Grammar::Use::register_internal_module( 'Digest::SHA1' => 'Perlito5X::Java::Digest::SHA1' );
    Perlito5::Grammar::Use::register_internal_module( 'Encode'       => 'Perlito5X::Java::Encode' );
    # Perlito5::Grammar::Use::register_internal_module( 'JSON'         => 'Perlito5X::Java::JSON' );
    Perlito5::Grammar::Use::register_internal_module( 'Time::HiRes'  => 'Perlito5X::Java::Time::HiRes' );
}

1;


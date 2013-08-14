
use strict;
use warnings;

use Test::More tests => 2;

BEGIN { use_ok('Some::Thing') };

my $string = Some::Thing::foo();

is( $string, 'Hello World' );


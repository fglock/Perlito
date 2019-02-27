use strict;
use warnings;
use Test::More tests => 2;
use lib 't';

{ package Clean; use Foo; }

is_deeply([ @Clean::ISA ], [], "Didn't mess with caller's ISA");
is(Clean->can('has'), undef, "Didn't export anything");

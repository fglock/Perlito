use Test::More tests => 4;

use lib 't';
use Bar;

my $b = Bar->new;

ok $b->isa('Foo'), 'Bar is a subclass of Foo';

is "@Bar::ISA", "Foo", 'Extends with multiple classes not supported';

ok 'Foo'->can('stuff'), 'Foo is loaded';
ok not('Bar'->can('buff')), 'Boo is not loaded';

use Test::More;

plan tests => 2;

$main::count = 1;

package Foo;
use Mo 'build';
has 'foo' => (is => 'rw');
sub BUILD {
    my $self = shift;
    $self->foo($main::count++);
}

package Bar;
use Mo;
extends 'Foo';
has 'bar' => (is => 'rw');

package Baz;
use Mo;
extends 'Bar';
has 'baz' => (is => 'rw');
sub BUILD {
    my $self = shift;
    $self->baz($main::count++);
}

package Gorch;
use Mo;
extends 'Baz';
has 'gorch' => (is => 'rw');

package main;

my $g = Gorch->new;
is $g->foo, 1, 'foo builds first';
is $g->baz, 2, 'baz builds second';

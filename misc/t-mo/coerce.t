use Test::More;

plan tests => 2;

package Foo::coerce;
use Mo qw(coerce);

has 'stuff' => (coerce => sub { uc $_[0] });

package main;

my $f = Foo::coerce->new(stuff => 'fubar');
is $f->stuff, 'FUBAR', 'values passed to constructor are successfully coerced';
$f->stuff('barbaz');
is $f->stuff, 'BARBAZ', 'values passed to setters are successfully coerced';

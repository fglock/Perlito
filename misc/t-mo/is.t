use Test::More;

plan tests => 2;

package Foo::is;
use Mo qw(is);

has 'stuff' => (is => 'ro');

package main;

my $f = Foo::is->new(stuff => 'foo');
is $f->stuff, 'foo', 'values passed to constructor are successfully accepted';
eval { $f->stuff('barbaz') };
like $@, qr{^stuff is ro}, 'setting values after initialization throws an exception';

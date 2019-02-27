use Test::More;

plan tests => 5;

#============
package Foo::required;
use Mo qw(required);

has 'stuff' => (required => 1);
has 'stuff2' => (required => 1);
has 'foo' => ();
#============
package Foo::required_is;
use Mo qw(required is);

has 'stuff' => (required => 1, is => 'ro');
#============

package main;

my $f0 = eval { Foo::required->new(stuff2 => 'foobar') };
like $@, qr/^stuff required/, 'Mo dies when a required value is not provided';

my $f = Foo::required->new(stuff => 'fubar', stuff2 => 'foobar');
is $f->stuff, 'fubar', 'Object is correctly initialized when required values are provided';

my $f1 = Foo::required->new(stuff => undef, stuff2 => 0);
is $f1->stuff, undef, 'Required parameters can be undef';
is $f1->stuff2, 0, 'Required parameters can be 0';

my $f2 = Foo::required_is->new(stuff => 'fubar');
is $f2->stuff, 'fubar', 'Object is correctly initialized when required is combined with is';

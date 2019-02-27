use Test::More;

plan tests => 25;

package Test::Chain;
use Mo qw(chain);

has 'first'  => (chain => 1);
has 'second' => (chain => 1);
has 'third'  => (chain => 0);


package Test::ChainWithDefault;
use Mo qw(chain default);

has 'first'  => (chain => 1, default => sub {11});
has 'second' => (chain => 1, default => sub {12});
has 'third'  => (chain => 0, default => sub {13});


package main;

### just chain
my $f = Test::Chain->new;

# not defined
is $f->first, undef;
is $f->third, undef;

# setter
isa_ok $f->first('foo'), 'Test::Chain';
isa_ok $f->second('bar'), 'Test::Chain';
is $f->third('baz'), 'baz';

# getter
is $f->first, 'foo';
is $f->second, 'bar';
is $f->third, 'baz';

# chain!
is $f->first(1)->second(2)->third(3), 3;
is $f->first, 1;
is $f->second, 2;

# chain with set to false
is $f->first(0)->second(undef)->third(''), '';
is $f->first, 0;
is $f->second, undef;

### chain with default
$f = Test::ChainWithDefault->new;

# not defined
is $f->first, 11;
is $f->third, 13;

# setter
isa_ok $f->first('21'), 'Test::ChainWithDefault';
isa_ok $f->second('22'), 'Test::ChainWithDefault';
is $f->third('23'), '23';

# getter
is $f->first, '21';
is $f->second, '22';
is $f->third, '23';

# chain!
is $f->first(31)->second(32)->third(33), 33;
is $f->first, 31;
is $f->second, 32;

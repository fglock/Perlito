use Test::More tests => 34;
my $call_count;

package Baz;
use Mo qw(default is);
has foo => { less => 'Mo' };
has fu  => {};
has bar => '';
has baz => 123;
has jar => [ 4, 2, 42 ];
has mar => [];

has something => ( default => sub { [] } );
has anything  => ( default => sub { +{} } );
has nothing   => ( default => sub { 0 } );

has past => ( default => sub { [ 1, 2, 3 ] } );
has present => ( default => sub { +{ a => 'A', b => 'B', C => 'c' } } );
has future => ( default => sub { 42 } );

has drink => ( 66, is => 'ro' );
has drank => ( [99], is => 'rw' );
has drunk => ( { hang => 'over' }, is => 'rw' );

has learned => sub { $call_count++; 'nothing' }, lazy => 0;

has fate => ();

package main;

my $eager = Baz->new;
is $call_count, 1, 'Eagerly initialized when asked';
is $eager->{learned}, 'nothing', "initialized correctly";
$eager = Baz->new(learned => "Abre-te sesamo");
is $call_count, 1, 'Default generator not called if initial value given to constructor';
is $eager->learned, 'Abre-te sesamo', "Attribute set to passed-in value";
is $call_count, 1, "default generator not called on the accessor";

# Regulars
my $foo = new_ok('Baz');
is_deeply $foo->something, [], 'Empty array';
is_deeply $foo->anything, {}, 'Empty hash';
is $foo->nothing, 0, 'Falseish Scalar';
is_deeply $foo->past, [ 1, 2, 3 ], 'Non empty array';
is_deeply $foo->present, { a => 'A', b => 'B', C => 'c' }, 'Non empty hash';
is $foo->future, 42, 'The answer to life the universe and everything';
is $foo->fate, undef, 'all the little unknowns..';

# Terse
is $foo->bar, '',  'Falseish default';
is $foo->baz, 123, 'Truish default';
is_deeply $foo->fu, {}, 'correct default';
is_deeply $foo->foo, { less => 'Mo' }, 'correct default';
is_deeply $foo->mar, [], 'correct default';
is_deeply $foo->jar, [ 4, 2, 42 ], 'correct default';
isnt( Baz->new->foo, Baz->new->foo, 'get new instances on every call' );
is_deeply( Baz->new->foo, Baz->new->foo, '.. but their content are the same' );

# Terse, irregulars
is $foo->drink, 66, 'recognize odd number of args';
is eval { $foo->drink(120) }, undef, 'recognize other args correctly';
like $@, qr/drink is ro/, 'recognize other args correctly';

is_deeply( $foo->drank, [99], 'odd args with array' );
is_deeply $foo->drank( { 33 => 66 } ), { 33 => 66 },
  'recognize other args correctly';
is_deeply $foo->drank, { 33 => 66 }, 'setter acts properly';

is_deeply( $foo->drunk, { hang => 'over' }, 'odd args with array' );

# Constructor arguments
$foo = new_ok( 'Baz', [ baz => 'changed', fu => 'none' ] );
is $foo->baz, 'changed', 'default can be overriden';
is $foo->fu,  'none',    'default can be overriden';

# Setters
$foo->past(undef);
$foo->present( {} );
$foo->future( [] );
is $foo->past, undef, 'set to undef';
is_deeply $foo->present, {}, 'set to empty hash';
is_deeply $foo->future, [], 'the future isnt bright';

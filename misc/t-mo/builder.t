use Test::More;
plan tests => 8;

{
    package Foo;
    use Mo qw'builder';
    has name => ( builder => 'simple' );
    has age  => ( builder => 'simple', lazy => 1 );
    has sex  => ( builder => 'simple', lazy => 0 );
    has needs => ( builder => 'complex' );

    sub simple { 'something' }
    sub complex { shift->name }
}
{
    package Bar;
    use Mo qw'builder nonlazy';
    has name => ( builder => 'simple' );
    has age  => ( builder => 'simple', lazy => 1 );
    has sex  => ( builder => 'simple', lazy => 0 );

    sub simple { 'something' }
}

package main;

my $foo = Foo->new;
is $foo->{name}, undef, 'attr are lazy by default';
is $foo->{age}, undef, 'attr are lazy when explicitly asked';
is $foo->{sex}, 'something', 'attr are eager when explicitly asked';
ok $foo->name('foobar'), 'setter ok';
is $foo->needs, 'foobar', 'builder receives $self correctly';

my $bar = Bar->new;
is $bar->{name}, 'something', 'attr are eager by default when nonlazy is imported';
is $bar->{age}, undef, 'attr are lazy when explicitly asked';
is $bar->{sex}, 'something', 'attr are eager when explicitly asked';

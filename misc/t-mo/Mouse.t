use Test::More;

BEGIN {
    eval "use Mouse";
    $@ and plan skip_all => "Mouse is needed";
};

plan tests => 5;

{
    package Foo;
    use Mo 'Mouse';

    has foo => ();
    has bar => (default => 'I like pie!');
}

is $Foo::ISA[0], 'Mouse::Object', 'ISA Mouse::Object';

my $f = Foo->new(foo => 42);

is $f->foo, 42, 'Normal';
is $f->{bar}, undef, 'before (lazy)';
is $f->bar, 'I like pie!', 'default';
is $f->{bar}, 'I like pie!', 'after';


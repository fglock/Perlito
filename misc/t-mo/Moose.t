use Test::More;

BEGIN {
    eval "use Moose";
    $@ and plan skip_all => "Moose is needed";
};

plan tests => 5;

{
    package Foo;
    use Mo 'Moose';

    has foo => ();
    has bar => (default => 'I like pie!');
}

is $Foo::ISA[0], 'Moose::Object', 'ISA Moose::Object';

my $f = Foo->new(foo => 42);

is $f->foo, 42, 'Normal';
is $f->{bar}, undef, 'before (lazy)';
is $f->bar, 'I like pie!', 'default';
is $f->{bar}, 'I like pie!', 'after';


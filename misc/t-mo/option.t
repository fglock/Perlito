use Test::More tests => 4;

{
    package Foo;
    use Mo qw(option);

    has 'this' => (option => 1);
    has 'that' => (option => 1);
}

### just chain
my $f = Foo->new;

# not defined
is $f->read_this, undef, 'Read this';
is $f->read_that, undef, 'Read that';

$f->this->that(0);

is $f->read_this, 1, 'Option on';
is $f->read_that, 0, 'Option off';

#!./perl -w

use feature 'say';

say "1..7";

package Class;

sub new
{
    return bless {'foo' => [100,200,300], baz => {a => 2} }, 'Class';
}

sub foo
{
    my $self = shift;
    return $self->{foo};
}

sub baz
{
    my $self = shift;
    return $self->{baz};
}

package main;

{
    my $obj1 = Class->new;

    $obj1->foo->[2] = undef();

    print "not " if (defined($obj1->{'foo'}->[2]));
    say ("ok 1 - undef on dereference is working.");
}

{
    my $obj2 = Class->new;

    undef ($obj2->foo->[2]);

    print "not " if (defined($obj2->{'foo'}->[2]));
    say ("ok 2 - undef(\$obj->x) on dereference is working.");
}

{
    my $obj2 = Class->new;

    undef (${$obj2->foo}[2]);

    print "not " if (defined($obj2->{'foo'}->[2]));
    say ("ok 3 - undef(\$obj->x) on dereference is working.");
}

{
    my $obj2 = Class->new;

    delete ($obj2->foo->[2]);

    print "not " if (exists($obj2->{'foo'}->[2]));
    say ("ok 4 - delete(\$obj->x) on dereference is working.");
}

{
    my $obj2 = Class->new;

    delete (${$obj2->foo}[2]);

    print "not " if (exists(${$obj2->{'foo'}}[2]));
    say ("ok 5 - delete(\$obj->x) on dereference is working.");
}

{
    my $obj2 = Class->new;

    delete ($obj2->baz->{a});

    print "not " if (exists($obj2->{'baz'}->{a}));
    say ("ok 6 - delete(\$obj->x) on dereference is working.");
}

{
    my $obj2 = Class->new;

    delete (${$obj2->baz}{a});

    print "not " if (exists(${$obj2->{'baz'}}{a}));
    say ("ok 7 - delete(\$obj->x) on dereference is working.");
}


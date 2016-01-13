#!./perl -w

BEGIN {
    chdir 't' if -d 't';
    @INC = '../lib';
    require './test.pl';
}

plan( tests => 2 );

package Class;

sub new
{
    return bless {'foo' => [100,200,300]}, 'Class';
}

sub foo
{
    my $self = shift;
    return $self->{foo};
}

package main;

{
    my $obj1 = Class->new;

    $obj1->foo->[1] = undef();

    # TEST
    ok (!defined($obj1->{'foo'}->[1]), "undef on dereference is working.");
}

{
    my $obj2 = Class->new;

    undef ($obj2->foo->[1]);

    # TEST
    ok (!defined($obj2->{'foo'}->[1]), "undef(\$obj->x) on dereference is working.");
}

#!./perl -w

use feature 'say';

say "1..2";

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
    print "not " if (defined($obj1->{'foo'}->[1]));
    say ("ok 1 - undef on dereference is working.");
}

{
    my $obj2 = Class->new;

    undef ($obj2->foo->[1]);

    # TEST
    print "not " if (defined($obj2->{'foo'}->[1]));
    say ("ok 2 - undef(\$obj->x) on dereference is working.");
}

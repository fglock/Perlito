#!./perl -w

use feature 'say';

say "1..2";

package Class;

sub new
{
    return bless {'foo' => 24, 'bar' => 100}, 'Class';
}

sub plus_one
{
    my $self = shift;
    my $arg = shift;

    return $self->{$arg} + 1;
}

sub times_three
{
    my $self = shift;
    my $arg = shift;

    return $self->{$arg} * 3;
}

package main;

{
    my $obj1 = Class->new;

    # TEST
    print "not " if ($obj1->can('plus_one')->($obj1, 'foo') != 25);
    say ("ok 1 - can works");

    # TEST
    print "not " if ($obj1->can('times_three')->($obj1, 'bar') != 300);
    say ("ok 2 - can works (#2)");
}


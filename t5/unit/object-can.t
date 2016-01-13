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
    is ($obj1->can('plus_one')->($obj1, 'foo'), 25, "->can works");

    # TEST
    is ($obj1->can('times_three')->($obj1, 'bar'), 300, "->can works (#2)");
}

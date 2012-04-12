use feature 'say';

package Other;

sub new {
    my $class = shift;
    bless { @_ }, $class
}

sub subr {
    my $self = shift;
    my $a = shift;
    my $b = shift;
    $a + $b
}

sub subr2 {
    my $self = shift;
    my $a = shift;
    my $b = shift;
    my $c = shift;
    $a + $b + $c
}

sub my_accessor { $_[0]->{my_accessor} + 1 }

package Main;
    
    say '1..5';

    my $other = Other->new();

    my $x = 0;
    $x = $other->subr( 1, 2 );
    if ($x != 3) {
        print 'not '
    }
    say 'ok 1 - ', $x;



    $x = 0;
    $x = Other->subr2( 1, 2, 4 );
    if ($x != 7) {
        print 'not '
    }
    say 'ok 2 - ', $x;


    my $obj = Other->new( my_accessor => '123' );
    print 'not ' if $obj->{my_accessor} ne '123';
    say 'ok 3 - ', $obj->{my_accessor};

    print 'not ' if $obj->my_accessor ne '124';
    say 'ok 4 - ', $obj->my_accessor;

    print 'not ' if ref($obj) ne 'Other';
    say 'ok 5 - ref # ', ref($obj);


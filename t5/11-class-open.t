
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

package Other;

sub subr2 {
    my $self = shift;
    my $a = shift;
    my $b = shift;
    my $c = shift;
    $a + $b + $c
}

package Main;
    
    say '1..2';

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


use feature 'say';
use strict;

package OtherInner;

our $AUTOLOAD;

sub AUTOLOAD {
    say "# $AUTOLOAD - @_";
    my $self = shift;
    my $a    = shift;
    my $b    = shift;
    my $c    = shift;
    $a + $b + $c
}

package Other;

our @ISA = ("OtherInner");

sub new {
    my $class = shift;
    bless {@_}, $class;
}

sub subr {
    my $self = shift;
    my $a    = shift;
    my $b    = shift;
    $a + $b;
}

sub my_accessor { $_[0]->{my_accessor} + 1 }

package Main;

say '1..6';

my $other = Other->new();

my $x = 0;
$x = $other->subr( 1, 2 );
if ( $x != 3 ) {
    print 'not ';
}
say 'ok 1 - ', $x;

$x = 0;
$x = Other->subr2( 1, 2, 4 );
if ( $x != 7 ) {
    print 'not ';
}
say 'ok 2 - ', $x;

print 'not ' if Other->isa("main");
say 'ok 3 - isa main';

print 'not ' if !Other->isa("OtherInner");
say 'ok 4 - isa main';

print 'not ' if !$other->isa("OtherInner");
say 'ok 5 - isa OtherInner';

print 'not ' if !$other->isa("Other");
say 'ok 6 - isa Other';


use strict;
use feature 'say';

package Other;
    sub new1 {
        my $class = shift;
        my %params = @_;
        my $self = \%params;
        bless $self, $class;
    }
    sub subr { say 'ok ', $_[0]->{'a'} };


package Main;
    
    say '1..3';
    say 'ok 1 - load ok';

    my $other = Other->new1( 'a' => 2 );
    $other->subr();

    $other->{'a'} = 3;
    say 'ok ', $other->{'a'};


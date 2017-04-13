use v5;
use strict;
use feature 'say';

{
    package TheScalar;
    
    sub TIESCALAR {
        my $class = shift;
        say "# TIESCALAR $class";
        bless { @_ }, $class
    }
    
    sub FETCH { 
        my $self = shift;
        say "# FETCH ";
        return $self->{'zero'};
    }
    
    sub STORE { 
        my $self = shift;
        my $v = shift;
        $self->{zero} = $v;
        say "# STORE $v";
    }

    sub UNTIE {
        say "# UNTIE";
    }
}

say '1..1';

my $s;

tie $s, 'TheScalar';

$s = 'first';

if ($s ne 'first') {
    print 'not '
};
say 'ok 1 # ';

untie $s;


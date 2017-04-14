use v5;
use strict;
use feature 'say';

say '1..3';

our $counter;
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
        $counter++;
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

my $s;

$s = 'before';

tie $s, 'TheScalar';

$s = undef;

if ($s ne '') {
    print 'not '
};
say 'ok 1 # ';

if ($counter ne 1) {
    print 'not '
};
say 'ok 2 # fetch called';

# autovivify an array
$s->[0] = 123;

print "# $s\n";

if (ref($s) ne 'ARRAY') {
    print 'not '
};
say 'ok 3 # ';



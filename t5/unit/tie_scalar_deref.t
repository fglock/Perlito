use v5;
use strict;
use feature 'say';

say '1..4';

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

sub ti {
    tie ${$_[0]}, 'TheScalar';
}

ti(\$s);


$s = 'first';

if ($s ne 'first') {
    print 'not '
};
say 'ok 1 # ';

if ($counter ne 1) {
    print 'not '
};
say 'ok 2 # fetch called';

my $t = tied $s;
if (ref($t) ne "TheScalar") {
    print 'not '
};
say 'ok 3 # tied';

untie $s;

if ($s ne 'first') {
    print 'not '
};
say "ok 4 # $s";



use v5;
use strict;
use feature 'say';

{
    package TheHash;
    
    sub TIEHASH {
        my $class = shift;
        say "# TIEHASH $class";
        bless { @_ }, $class
    }
    
    sub FETCH { 
        my $self = shift;
        my $i = shift;
        say "# FETCH $i";
        if ($i eq "0") {
            return $self->{'zero'};
        }
    }
    
    sub STORE { 
        my $self = shift;
        my $i = shift;
        my $v = shift;
        say "# STORE $i, $v";
        if ($i eq "0") {
            $self->{'zero'} = $v;
            return;
        }
        if ($i eq "1") {
            say $v;
            return;
        }
    }

    sub UNTIE {
        say "# UNTIE";
    }
}

say '1..2';

my %hash;

tie %hash, 'TheHash';

$hash{0} = 'first';

if ($hash{0} ne 'first') {
    print 'not '
};
say 'ok 1 # ', $hash{0};

$hash{1} = 'ok 2 # whatever';

untie %hash;


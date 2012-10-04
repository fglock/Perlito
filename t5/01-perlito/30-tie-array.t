use v5;
use strict;
use feature 'say';

{
    package TheArray;
    
    sub TIEARRAY {
        my $class = shift;
        say "# TIEARRAY $class";
        bless { @_ }, $class
    }
    
    sub FETCH { 
        my $self = shift;
        my $i = shift;
        say "# FETCH $i";
        if ($i == 0) {
            return $self->{'zero'};
        }
    }
    
    sub STORE { 
        my $self = shift;
        my $i = shift;
        my $v = shift;
        say "# STORE $i, $v";
        if ($i == 0) {
            $self->{'zero'} = $v;
            return;
        }
        if ($i == 1) {
            say $v;
            return;
        }
    }

    sub UNTIE {
        say "# UNTIE";
    }
}

say '1..2';

my @list;

tie @list, 'TheArray';

$list[0] = 'first';

if ($list[0] ne 'first') {
    print 'not '
};
say 'ok 1 # ', $list[0];

$list[1] = 'ok 2 # whatever';

untie @list;


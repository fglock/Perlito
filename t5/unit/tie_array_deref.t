use v5;
use strict;
use feature 'say';

# use Data::Dumper;

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
        if ($i == 1) {
            return $self->{'one'};
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
            $self->{'one'} = $v;
            # say $v;
            return;
        }
    }

    sub SHIFT {
        return "shift ok";
    }

    sub FETCHSIZE { 
        my $self = shift;
        say "# FETCHSIZE";
        return 2;
    }
 
    sub UNTIE {
        say "# UNTIE";
    }
}

say '1..2';

my @array;


sub ti {
    tie @{$_[0]}, 'TheArray';
}

ti(\@array);

print 'not ' if ! tied(@array);
say "ok 1 - tied # ", tied(@array);

$array[0] = 3;
$array[1] = 4;

my @out;
while ( my ( $key, $value ) = each @array ) {
    push @out, ( $key, $value );
}

print 'not ' if "@out" ne "0 3 1 4";
say "ok 2 - while-each # @out";



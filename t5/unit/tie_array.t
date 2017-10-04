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

    sub SHIFT {
        return "shift ok";
    }

    sub UNTIE {
        say "# UNTIE";
    }
}

# use subs 'shift';
# sub shift               { say "not ok 100 # PKG::shift()" }
# sub CORE::shift (;+)    { say "not ok 101 # CORE::shift()" }
# sub CORE::GLOBAL::shift { say "not ok 102 # CORE::GLOBAL::shift()" }
# BEGIN {
#     *shift = sub { say "not ok 103 # PKG::shift()" }
# }

say '1..3';

my @list;

tie @list, 'TheArray';

$list[0] = 'first';

if ($list[0] ne 'first') {
    print 'not '
};
say 'ok 1 # ', $list[0];

$list[1] = 'ok 2 # whatever';

my $v = shift @list;
print "not " unless $v eq 'shift ok';
say "ok 3 # shift $v";

untie @list;

shift @list;


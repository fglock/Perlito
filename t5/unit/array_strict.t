use v5;
use strict;
use feature 'say';

say '1..3';
my @a;

    {
    no strict;
    # $ perl -e ' use Data::Dumper;  @$a; print Dumper $a; '
    $a = undef;
    my @x = @$a;
    if (ref($a) eq 'ARRAY') {
        print 'not '
    }
    say 'ok 1 - deref array is lazy';
    }

    {
    use strict;
    # $ perl -e ' use Data::Dumper;  @$a; print Dumper $a; '
    local $@;
    $a = undef;
    eval { my @x = @$a; };
    if (substr($@,0,20) ne substr("Can't use an undefined value as an ARRAY reference",0,20)) {
        print 'not '
    }
    say 'ok 2 - deref array is forbidden';
    }

    {
    no strict;
    # $ perl -e ' use Data::Dumper;  @$a; print Dumper $a; '
    local $@;
    $a = undef;
    eval { my @x = @$a; };
    if (substr($@,0,20) eq substr("Can't use an undefined value as an ARRAY reference",0,20)) {
        print 'not '
    }
    say 'ok 3 - deref array is not forbidden under no strict';
    }



use feature 'say';

say '1..3';

{
    package X;

    our @x;
    @x = (4,5,6);
    push @X::x, 7;
    print 'not ' unless "@x" eq "4 5 6 7";
    say "ok 1 - 'our' array variable [@x] [@X::x]";

    our %h;
    %h = (4,5);
    $X::h{6} = 7;
    print 'not ' unless "@{[ %h ]}" eq "4 5 6 7" || "@{[ %h ]}" eq "6 7 4 5";
    say "ok 2 - 'our' hash variable [@{[ %h ]}] [@{[ %X::h ]}]";

    our $s;
    $s = 4;
    $X::s = 5;
    print 'not ' unless $s == 5;
    say "ok 3 - 'our' scalar variable [$s]";
}


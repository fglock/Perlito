use feature 'say';

print "1..8\n";

my $v = 0;
my $r = 1;
my $x = 2;
my $e;

{
    package P;
    use strict; 
    $r = 3;
    $v = 5;
    sub A { $x = $_[0]; $r = 4 }
    sub B { $v = 6 }
    A B; 
    print "not " if $r != 4;
    say "ok 1 - sub call # $r";    
    print "not " if $v != 6;
    say "ok 2 - sub call # $v";    

    $r = 3;
    A P::;
    print "not " if $r != 4;
    say "ok 3 - method call # $r";    
    print "not " if !$x;
    say "ok 4 - method call # $x";    

    $r = 3;
    # TODO - 'A P' without eval;
    $e = eval 'A P; 1';
    print "not " if $r != 4;
    say "ok 5 - method call # $r";    
    print "not " if !$x;
    say "ok 6 - method call # $x";    
    print "not " if !$e;
    say "ok 7 - syntax ok # $e " . ($@ ? $@ : '');    

    $r = 3;
    $e = eval 'A Q; 1';  # Bareword "Q" not allowed
    print "not " if $r != 3;
    say "ok 8 - syntax error - $r # TODO - strict";    
    print "not " if $e;
    say "ok 9 - syntax error - $e " . ($@ ? substr($@, 0, 20) : '') . " # TODO - strict";    
}

{
    no strict;

    $r = 3;
    eval 'A Q';  # this would be an error: Bareword "Q" not allowed
    print "not " if $r != 3;
    say "ok 10 - not a syntax error # $r";    
}


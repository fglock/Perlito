use feature 'say';

say "1..21";

my $v = 0;
my $r = 1;
my $x = 2;
my $e;

{
    package M;
    use strict; 
    sub A { $x = $_[0]; $r = 12 }
    sub C { $v = 13 }
}

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
    say "ok 9 - syntax error - $e " . ($@ ? substr($@, 0, 30) : '') . " # TODO - strict";    

    $r = 3;
    $e = eval 'A Q::; 1';  # Can't locate object method "A"
    print "not " if $r != 3;
    say "ok 10 - runtime error - $r # TODO - strict";    
    print "not " if $e;
    say "ok 11 - runtime error - $e " . ($@ ? substr($@, 0, 30) : '') . " # TODO - strict";    

    $r = 3;
    $e = eval 'A M; 1';  # Bareword "M" not allowed
    print "not " if $r != 12;
    say "ok 12 - method in other package - $r # TODO - strict";    
    print "not " if !$e;
    say "ok 13 - method in other package - $e " . ($@ ? substr($@, 0, 30) : '') . " # TODO - strict";    

    $r = 3;
    $e = eval 'A M::; 1';  # Can't locate object method "A"
    print "not " if $r != 12;
    say "ok 14 - method in other package - $r # TODO - strict";    
    print "not " if !$e;
    say "ok 15 - method in other package - $e " . ($@ ? substr($@, 0, 30) : '') . " # TODO - strict";    


    $v = 3;
    $e = eval 'C M; 1';  # Bareword "M" not allowed
    print "not " if $v != 13;
    say "ok 16 - method in other package - $v # TODO - strict";    
    print "not " if !$e;
    say "ok 17 - method in other package - $e " . ($@ ? substr($@, 0, 30) : '') . " # TODO - strict";    

    $v = 3;
    $e = eval 'C M::; 1';  # Can't locate object method "C"
    print "not " if $v != 13;
    say "ok 18 - method in other package - $v ";    
    print "not " if !$e;
    say "ok 19 - method in other package - $e " . ($@ ? substr($@, 0, 30) : '') . " ";    

}

{
    package R;
    no strict;

    $r = 3;
    $e = eval 'A Q; 1';  # this would be an error: Bareword "Q" not allowed
    print "not " if $r != 3;
    say "ok 20 - bareword not a syntax error under no strict # $r";    
    print "not " if $e;
    say "ok 21 - bareword; runtime error - $e " . ($@ ? substr($@, 0, 30) : '') . " #";  
}


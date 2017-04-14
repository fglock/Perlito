use feature 'say';

say '1..2';

${ x ->{s} } = 4;

my ($k) = keys %x;

print 'not ' unless $k eq "s";
say "ok 1 - hash deref set # $k";

{
    my %x;

    ${ x ->{t} } = 4;
    
    my ($k) = keys %x;
    
    print 'not ' if $k && $k eq "t";
    say "ok 2 - hash deref variable is always global # $k";
}


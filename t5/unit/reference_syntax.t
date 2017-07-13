use feature 'say';

say '1..4';

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


# without arrow

${ z {s} } = 4;

my ($k) = keys %z;

print 'not ' unless $k eq "s";
say "ok 3 - hash deref set # $k";

{
    my %z;

    ${ z {t} } = 4;
    
    my ($k) = keys %z;
    
    print 'not ' if !($k && $k eq "t");
    say "ok 4 - hash deref variable is not global # $k";
}


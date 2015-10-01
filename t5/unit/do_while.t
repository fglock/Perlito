use feature 'say';

say '1..5';

$_ = 1;
{
    say "ok 1 - inside block";
    do {
        $_++;
        next if $_ == 1 || $_ == 3;
        say "ok $_ - next in do-while goes to outer block";
    } while ( $_ < 5 );
}

say "ok 3 - outside block";
do {
    {
        $_++;
        next if $_ == 1 || $_ == 3;
        say "ok $_ - next in do-while in inner block";
    }
} while ( $_ < 5 );


use v5;
use strict;
use feature 'say';

say '1..71';

my $x = "abcd";
if (substr($x,1,1) ne "b") {
    print 'not '
};
say 'ok 1 - substr ', substr($x,1,1);

if (index($x,"c") ne 2) {
    print 'not '
};
say 'ok 2 - index ', index($x,"c");

if (substr($x,3,1) ne "d") {
    print 'not '
}
say 'ok 3 - substr ', substr($x,3,1);

print 'not ' if !defined "abc";
say 'ok 4 - defined str';

my $s = "o";
$s .= "k 5 - concat";
say $s;

$s = "The black cat climbed the green tree";
my $color  = substr $s, 4, 5;      # black
my $middle = substr $s, 4, -11;    # black cat climbed the
my $end    = substr $s, 14;        # climbed the green tree
my $tail   = substr $s, -4;        # tree
my $z      = substr $s, -4, 2;     # tr

say "# $s";

print 'not ' if $color ne 'black';
say "ok 6  # $color";

print 'not ' if $middle ne 'black cat climbed the';
say "ok 7  # $middle";

print 'not ' if $end ne 'climbed the green tree';
say "ok 8  # $end";

print 'not ' if $tail ne 'tree';
say "ok 9  # $tail";

print 'not ' if $z ne 'tr';
say 'ok 10';


# interpolation

my $v = 123;
my $r = "-$v-";
print 'not ' if $r ne '-123-';  say 'ok 11 - scalar interpolation';

my @v = (234, 567);
$r = "-$v[1]-";
print 'not ' if $r ne '-567-';  say 'ok 12 - array element interpolation';

$r = "-${v[1]}-";
print 'not ' if $r ne '-567-';  say 'ok 13 - array element interpolation';

$r = "-@v-";
print 'not ' if $r ne '-234 567-';  say 'ok 14 - array interpolation';

my %v = (xyz => 234, abc => 567);
$r = "-$v{xyz}-";
print 'not ' if $r ne '-234-';  say 'ok 15 - hash element interpolation';

$r = "-${v{xyz}}-";
print 'not ' if $r ne '-234-';  say 'ok 16 - hash element interpolation';

$v = { xyz => 123, abc => 567 };
$r = "-$v->{xyz}-";
print 'not ' if $r ne '-123-';  say "ok 17 - hash deref interpolation - $r";

# {
#     no strict 'refs';
#     # Can't use bareword ("v") as a HASH ref while "strict refs" in use 
#     # Global symbol "%v" requires explicit package name 
#     $r = "-${v->{xyz}}-";
#     print 'not ' if $r ne '-234-';  say 'ok 18 - hash deref interpolation';
# }

$v = [ 123, 567, 890 ];
$r = "-$v->[2]-";
print 'not ' if $r ne '-890-';  say "ok 18 - array deref interpolation - $r";

# {
#     no strict 'refs';
#     # Can't use bareword ("v") as a HASH ref while "strict refs" in use 
#     # Global symbol "@v" requires explicit package name 
#     $r = "-${v->[2]}-";
#     print 'not ' if $r ne '-890-';  say 'ok 18 - array deref interpolation';
# }

$r = "-$$v[2]-";
print 'not ' if $r ne '-890-';  say "ok 19 - array deref interpolation - $r";

{
    my $x = "123"; 
    my $y = \$x; 
    $r = "[$$y]";
    print 'not ' if $r ne '[123]';  say "ok 20 - scalar deref interpolation - $r";
}

# autoincrement

$v = 'AZ';
$r = $v++;
print 'not ' if $r ne 'AZ';  say "ok 21 - string increment - $r";
print 'not ' if $v ne 'BA';  say "ok 22 - string increment - $v";

$v = 'Z';
$r = ++$v;
print 'not ' if $r ne 'AA';  say "ok 23 - string increment - $r";
print 'not ' if $v ne 'AA';  say "ok 24 - string increment - $v";

# autodecrement

$v = 'AZ';
$r = $v--;
print 'not ' if $r ne 'AZ';  say "ok 25 - string decrement - $r";
print 'not ' if $v ne '-1';  say "ok 26 - string decrement - $v";

$v = 'Z';
$r = --$v;
print 'not ' if $r ne '-1';  say "ok 27 - string decrement - $r";
print 'not ' if $v ne '-1';  say "ok 28 - string decrement - $v";

$v = '5AZ';
$r = $v--;
print 'not ' if $r ne '5AZ'; say "ok 29 - string decrement - $r";
print 'not ' if $v ne '4';   say "ok 30 - string decrement - $v";

$v = '-5Z';
$r = --$v;
print 'not ' if $r ne '-6';  say "ok 31 - string decrement - $r";
print 'not ' if $v ne '-6';  say "ok 32 - string decrement - $v";

# unary minus

$v = 'AZ';
$r = -$v;
print 'not ' if $r ne '-AZ';  say "ok 33 - string negative - $r";
print 'not ' if $v ne 'AZ';   say "ok 34 - string negative - $v";

$v = '-Z';
$r = -$v;
print 'not ' if $r ne '+Z';  say "ok 35 - string negative - $r";
print 'not ' if $v ne '-Z';  say "ok 36 - string negative - $v";

$v = '-NAN';
$r = -$v;
print 'not ' unless $r eq '+NAN' || $r eq 'nan' || $r eq 'NaN';  
                             say "ok 37 - string negative - $r";

$v = '-INF';
$r = -$v;
print 'not ' unless $r eq '+INF' || $r eq 'inf' || $r eq 'Inf'; 
                             say "ok 38 - string negative - $r";


$v = ' AZ ';
$r = -$v;
print 'not ' unless $r eq '-0' || $r eq '0';  
                             say "ok 39 - string negative - $r";

$v = ' -Z ';
$r = -$v;
print 'not ' if $r ne '0';  say "ok 40 - string negative - $r";

$v = ' - ';
$r = -$v;
print 'not ' if $r ne '0';  say "ok 41 - string negative - $r";

$v = ' + ';
$r = -$v;
print 'not ' unless $r eq '-0' || $r eq '0'; 
                             say "ok 42 - string negative - $r";

$v = '-';
$r = -$v;
print 'not ' if $r ne '+';  say "ok 43 - string negative - $r";

$v = '+';
$r = -$v;
print 'not ' if $r ne '-';  say "ok 44 - string negative - $r";

$v = '!AZ ';
$r = -$v;
print 'not ' unless $r eq '-0' || $r eq '0'; 
                             say "ok 45 - string negative - $r";

$v = '-!Z ';
$r = -$v;
print 'not ' if $r ne '+!Z ';  say "ok 46 - string negative - $r";

$v = '+!Z ';
$r = -$v;
print 'not ' if $r ne '-!Z ';  say "ok 47 - string negative - $r";

$v = '- 00.12';
$r = -$v;
print 'not ' if $r ne '+ 00.12';  say "ok 48 - string negative - $r";

$v = '-00.12';
$r = -$v;
print 'not ' unless $r eq '0.12' || $r eq '+00.12'; 
                             say "ok 49 - string negative - $r";


# unary plus

$v = 'AZ';
$r = +$v;
print 'not ' if $r ne 'AZ';  say "ok 50 - string positive - $r";
print 'not ' if $v ne 'AZ';  say "ok 51 - string positive - $v";

$v = '-Z';
$r = +$v;
print 'not ' if $r ne '-Z';  say "ok 52 - string positive - $r";
print 'not ' if $v ne '-Z';  say "ok 53 - string positive - $v";


# controls: \x

$r = "\x50a";
print 'not ' if $r ne 'Pa';  say "ok 54 - hex - $r";

$v = "50a";
$r = "\x$v";
$x = "\x{0}50a";
# print "# [", join( ", ", map { ord($_) } split //, $r ), "]\n";
# print "# [", join( ", ", map { ord($_) } split //, $v ), "]\n";
print 'not ' if $r ne $x;  say "ok 55 - hex without params - $r";

$r = "\x{50}a";
print 'not ' if $r ne 'Pa';  say "ok 56 - hex - $r";

$v = "{50}a";
$r = "\x$v";
$x = "\x{0}{50}a";
# print "# [", join( ", ", map { ord($_) } split //, $r ), "]\n";
# print "# [", join( ", ", map { ord($_) } split //, $x ), "]\n";
print 'not ' if $r ne $x;  say "ok 57 - hex - $r";

$r = "\x{}a";
$x = "\x{0}a";
# print "# [", join( ", ", map { ord($_) } split //, $r ), "]\n";
# print "# [", join( ", ", map { ord($_) } split //, $x ), "]\n";
print 'not ' if $r ne $x;  say "ok 58 - hex without params - $r";

# controls: \l \u

$r = "\lAA";
print 'not ' if $r ne 'aA';  say "ok 59 - lc $r";

$r = "\l\n";
print 'not ' if $r ne "\n";  say "ok 60 - lc";

$r = "\uaa";
print 'not ' if $r ne 'Aa';  say "ok 61 - uc $r";

$r = "\u\n";
print 'not ' if $r ne "\n";  say "ok 62 - uc";

$r = "\l\x50a";
print 'not ' if $r ne "pa";  say "ok 63 - lc";

$v = "\x50a";
$r = "\l$v";
$x = "pa";
# print "# [", join( ", ", map { ord($_) } split //, $r ), "]\n";
# print "# [", join( ", ", map { ord($_) } split //, $v ), "]\n";
print 'not ' if $r ne $x;  say "ok 64 - lc with var - $r";

$r = "\u\lAA\l\uaa\u\laa\l\uAA";
print 'not ' if $r ne 'AAaaAaaA';  say "ok 65 - lc $r";

# controls: \c

$r = "\cAA";
print 'not ' if $r ne "\x{01}A";  say "ok 66 - ctrl-A";

$r = "\caA";
print 'not ' if $r ne "\x{01}A";  say "ok 67 - ctrl-A";

$r = "\c0A";
print 'not ' if $r ne "pA";  say "ok 68 - ctrl-0";

# controls: \octal

$r = "\11111111";
# print "# [", join( ", ", map { ord($_) } split //, $r ), "]\n";
print 'not ' if $r ne "I11111";  say "ok 69 - octal - $r";

$r = "\1A";
print 'not ' if $r ne "\x{1}A";  say "ok 70 - octal";

# controls: \Q \L \U \E \l \u

$v = "123aaAA";
$r = "\Qa\U[{Exx${v}y\lXz\LC\n\E\n\E\n";
$x = 'a' . chr(92) . '[' . chr(92) . '{EXX' . quotemeta(uc($v)) . 'YXZc' . chr(92) . chr(10) . chr(92) . chr(10) . chr(10);
print 'not ' if $r ne $x;  say "ok 71 - nested controls";


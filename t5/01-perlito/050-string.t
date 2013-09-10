use v5;
use strict;
use feature 'say';

say '1..50';

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

# negative

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
print 'not ' if $r ne '+NAN';  say "ok 37 - string negative - $r";

$v = '-INF';
$r = -$v;
print 'not ' if $r ne '+INF';  say "ok 38 - string negative - $r";


$v = ' AZ ';
$r = -$v;
print 'not ' if $r ne '-0';  say "ok 39 - string negative - $r";

$v = ' -Z ';
$r = -$v;
print 'not ' if $r ne '0';  say "ok 40 - string negative - $r";

$v = ' - ';
$r = -$v;
print 'not ' if $r ne '0';  say "ok 41 - string negative - $r";

$v = ' + ';
$r = -$v;
print 'not ' if $r ne '-0';  say "ok 42 - string negative - $r";

$v = '-';
$r = -$v;
print 'not ' if $r ne '+';  say "ok 43 - string negative - $r";

$v = '+';
$r = -$v;
print 'not ' if $r ne '-';  say "ok 44 - string negative - $r";

$v = '!AZ ';
$r = -$v;
print 'not ' if $r ne '-0';  say "ok 45 - string negative - $r";

$v = '-!Z ';
$r = -$v;
print 'not ' if $r ne '+!Z ';  say "ok 46 - string negative - $r";




# positive

$v = 'AZ';
$r = +$v;
print 'not ' if $r ne 'AZ';  say "ok 47 - string positive - $r";
print 'not ' if $v ne 'AZ';  say "ok 48 - string positive - $v";

$v = '-Z';
$r = +$v;
print 'not ' if $r ne '-Z';  say "ok 49 - string positive - $r";
print 'not ' if $v ne '-Z';  say "ok 50 - string positive - $v";



#
#   $ touch Main.class
#   $ rm Main.class
#   $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava misc/Java/Test.pl > Main.java
#   $ javac Main.java
#   $ java Main
#
#   one liner:
#   $ touch Main.class ; rm Main.class ; perl perlito5.pl -Isrc5/lib -I. -It -Cjava misc/Java/Test.pl > Main.java ; javac Main.java ; java Main
#

use feature 'say';

print "1..105\n";
print "ok 1 - print() works\n";
say   "ok 2 - say() works";

my $x = 123;
$x = "ok 3";
say   "$x - scalar interpolation in string works";
$x = 4;
say   "ok $x - scalar number interpolation in string works";

Java::inline ' System.out.println("ok 5 - Java::inline works"); ';

my @aa = (5,7,8,6,9);
say "ok $aa[3] - array works";
my $a_key = 1;
say "ok $aa[$a_key] - array works";

my %hh = ( x => 8, y => 9 );
say "ok $hh{x} - hash works";
my $h_key = "y";
say "ok $hh{$h_key} - hash works";

my $ofs = 4;
my $aar = \@aa;
my $hhr = \%hh;
say "ok ", ($ofs + $aar->[3]      ), " - array ref works";
say "ok ", ($ofs + $aar->[$a_key] ), " - array ref works";
say "ok ", ($ofs + $hhr->{x}      ), " - hash ref works";
say "ok ", ($ofs + $hhr->{$h_key} ), " - hash ref works";

if (1) {
    say "ok 14 - if works"
}
else {
    say "not ok 14 - if works"
}

if (0) {
    say "not ok 15 - if works"
}
else {
    say "ok 15 - if works"
}

my $if = 1;
my $if2 = 2;
if ($if == 1) {
    say "ok 16 - if works"
}
else {
    say "not ok 16 - if works"
}
if ($if == 0) {
    say "not ok 17 - if works"
}
elsif ($if2 == 2) {
    say "ok 17 - if works"
}
else {
    say "not ok 17 - if works"
}

while ($if < 2) {
    say "ok 18 - while works";
    $if = $if + 1;
}
say "ok 19 - while finished";

my $s = "[@aa]";
if ($s ne "[5 7 8 6 9]") {
    print "not ";
}
say "ok 20 - array interpolation in string $s";

$s = @aa;
if ($s != 5) {
    print "not ";
}
say "ok 21 - array in scalar context $s";

$s = "[@{$aar}]";
if ($s ne "[5 7 8 6 9]") {
    print "not ";
}
say "ok 22 - arrayref interpolation in string $s";

$s = "[@{[ 2 * 2, 6 ]}]";
if ($s ne "[4 6]") {
    print "not ";
}
say "ok 23 - arrayref interpolation in string $s";

$s = "[@{[ %hh ]}]";
if ($s eq "[x 8 y 9]") {
    say "ok 24 - hash interpolation in array / string $s";
}
elsif ($s eq "[y 9 x 8]") {
    say "ok 24 - hash interpolation in array / string $s";
}
else {
    say "not ok 24 - hash interpolation in array / string $s";
}

$s = "[@{[ keys %hh ]}]";
if ($s eq "[x y]") {
    say "ok 25 - keys $s";
}
elsif ($s eq "[y x]") {
    say "ok 25 - keys $s";
}
else {
    say "not ok 25 - keys $s";
}

$s = "[@{[ values %hh ]}]";
if ($s eq "[8 9]") {
    say "ok 26 - values $s";
}
elsif ($s eq "[9 8]") {
    say "ok 26 - values $s";
}
else {
    say "not ok 26 - values $s";
}

$s = keys %hh;
if ($s != 2) {
    print "not ";
}
say "ok 27 - scalar keys $s";

$s = 3.45;

say "", ($s < 4 ? "" : "not "), "ok 28 - ternary op";

my @entry = each %hh;
if ($entry[0] eq "x") {
    say "ok 29 - each @entry";
}
elsif ($entry[0] eq "y") {
    say "ok 29 - each @entry";
}
else {
    say "not ok 29 - each @entry";
}

$s = "[" . @aa . "]";   # array in scalar context
if ($s ne "[5]") {
    print "not ";
}
say "ok 30 - array stringification $s";

my $aa = [5,32,8,31,9];
say "ok $aa->[3] - array literal works";
$a_key = 1;
say "ok $aa->[$a_key] - array literal works";

my $hh = { x => 33, y => 34 };
say "ok $hh->{x} - hash literal works";
$h_key = "y";
say "ok $hh->{$h_key} - hash literal works";


# import some native Java

package my::Sample { import => "java.util.Date" };

$x = my::Sample->new(); 

if (ref($x) ne 'my::Sample') {
    print "not ";
}
say "ok 35 - java object ref: ", ref($x), " ", $x;

# convert to native
my my::Sample $x_native = $x; 

my @arr = (1,2,5);
$x = \@arr;

if (ref($x) ne 'ARRAY') {
    print "not ";
}
say "ok 36 - array ref: ", ref($x), " ", $x;

# store native in scalar
$x = $x_native; 
if (ref($x) ne 'my::Sample') {
    print "not ";
}
say "ok 37 - store java object in scalar: ", ref($x), " ", $x;

my $sc;
$$sc = 5;   # $sc = \5;
if (ref($sc) ne 'SCALAR') {
    print "not ";
}
say "ok 38 - autovivify scalarref: ", ref($sc), " ", $sc;
if ($$sc != 5) {
    print "not ";
}
say "ok 39 - autovivify scalarref: ", $$sc;

@aa = qw/ x y z 123 4 /;
if ($aa[3] != 123) {
    print "not ";
}
say "ok 40 - qw(), join(): ", join( ", ", @aa );

# Test if these compile
@aa = ();
%hh = ();
$aa[1]{"a"} = 12;
$hh{"1"}{"a"} = 13;
$aa[2][1] = 14;
$hh{"2"}[1] = 15;

if (@aa != 3) { print "not " }
say "ok 41 - data structure";
if (keys(%hh) != 2) { print "not " }
say "ok 42 - data structure";

@main::aa = ();
%main::hh = ();

$aa = undef;
$hh = undef;
$aa->[1]{"a"} = 12;
$hh->{"1"}{"a"} = 13;
$aa->[2][1] = 14;
$hh->{"2"}[1] = 15;
if (@$aa != 3) { print "not " }
say "ok 43 - data structure";
if (keys(%$hh) != 2) { print "not " }
say "ok 44 - data structure";

$aa->[1]->{"a"} = 12;
$hh->{"1"}->{"a"} = 13;
$aa->[2]->[1] = 14;
$hh->{"2"}->[1] = 15;
if (@$aa != 3) { print "not " }
say "ok 45 - data structure";
if (keys(%$hh) != 2) { print "not " }
say "ok 46 - data structure";

{
my $a;
@$a = (10,20,30);
if (@$aa != 3) { print "not " }
say "ok 47 - data structure";
my $a;  # redeclaration
%$a = (a => 1, b => 2);
if (keys(%$hh) != 2) { print "not " }
say "ok 48 - data structure";
$a = undef;
$$a = 5;
if ($$a != 5) { print "not " }
say "ok 49 - data structure";

# TODO - test these
$main::a = undef;
@$main::a = (1,2,3);
$main::a = undef;
%$main::a = (a => 1, b => 2);
$main::a = undef;
$$main::a = 5;

# TODO - test these
@main::a = (20,30,40);
if (@main::a != 3) { print "not " }
say "ok 50 - data structure [ @main::a ]";
if ($main::a[1] != 30) { print "not " }
say "ok 51 - data structure [ @main::a ]";

# TODO - test these
@main::a = ();
@{ $main::a[1] } = (1,2,3);
%{ $main::a[2] } = (a => 1, b => 2);
${ $main::a[3] } = 5;
if (@main::a != 4) { print "not " }
say "ok 52 - data structure [ @main::a ]";

# TODO - test these
%main::a = ();
@{ $main::a{x1} } = (1,2,3);
%{ $main::a{x2} } = (a => 1, b => 2);
${ $main::a{x3} } = 5;
if (keys(%main::a) != 3) { print "not " }
say "ok 53 - data structure @{[ %main::a ]}";

# make sure interpolation of globals work
@main::a = ( @main::a, %main::a );
if (@main::a != 10) { print "not " }
say "ok 54 - data structure [ @main::a ]";
}

# closure
my $x = 3;
my $v = sub { $x = $x + 1; 1 + $x;  };
my $res = $v->() . " " . $x;
if ( $res ne "5 4" ) {
    print "not ";
}
say "ok 55 - closure [$res]";

# closure with params
$x = 3;
$v = sub { $x = $x + 1; $_[0] + $x;  };
$res = $v->(123) . " " . $x;
if ( $res ne "127 4" ) {
    print "not ";
}
say "ok 56 - closure with params [$res]";

$x = 3;
sub v { $x = $x + 1; $_[0] + $x;  }
$res = v(123) . " " . $x;
if ( $res ne "127 4" ) {
    print "not ";
}
say "ok 57 - subroutine [$res]";

# lazy boolean operators
$x = "0";
print "not " if $x;
say "ok 58 - boolean operator";

# lazy boolean operators
$x = "0E0";
print "not " unless $x;
say "ok 59 - boolean operator, 0E0";

say "ok 60 - boolean operator or die()"
    or die "didn't work";

my my::Sample $z = my::Sample->new();
# cast typed variable to Perl object automatically
my $x = $z;
say "ok 61 - initialize a Perl variable from a Java object: $x";
# initialize a typed variable by dereferencing a Perl object
my my::Sample $y = $x;
say "ok 62 - initialize a typed variable by dereferencing a Perl object: $x";

# my @things = my::Sample->lots_of_it();
# say "ok 63 - initialize an untyped Perl array with an array of object: [ @things ]";

say "ok 63 - [test removed]";

if (!defined $x) {
    print "not ";
}
say "ok 64 - defined()";
undef $x;
if (defined $x) {
    print "not ";
}
say "ok 65 - not defined()";

$x = 'ryba';
print 'not ' unless ( $x x 3 ) eq 'rybarybaryba';
say 'ok 66 - string replication';

my @a = ($x) x 3;
print 'not ' unless join( ',', @a ) eq 'ryba,ryba,ryba';
say "ok 67 - list replication in list context: [ @a ]";

my $c = scalar( ($x) x 3 );
print 'not ' unless $c == 3;
say "ok 68 - list replication in scalar context: [ @a ]";

@a = ( 1, 2, 3, 4 );
push @a, 7;
print 'not ' unless $a[4] == 7;
say "ok 69 - push an item to a list: [ @a ]";

@a = ( 1, 2, 3, 4 );
push @a, ( 7, 8, 9 );
print 'not ' unless $a[6] == 9;
say "ok 70 - push a list to a list: [ @a ]";

@a = ( 1, 2, 3, 4 );
push @a, 7, 8, 9;
print 'not ' unless $a[6] == 9;
say "ok 71 - push a list of items to a list: [ @a ]";

@a = ( 1, 2, 3 );
push @a, ( 4, 5 ), ( 6, 7 );
print 'not ' unless $a[6] == 7;
say "ok 72 - push a list of lists to a list: [ @a ]";

@a = ();
push @a, ( 4, 5 ), ( 6, 7 );
print 'not ' unless $a[3] == 7;
say "ok 73 - push a list of lists to an empty list: [ @a ]";

my @y = ('ryba', 'lufa');
my @a = (@y) x 2;
print 'not ' unless join( ',', @a ) eq 'ryba,lufa,ryba,lufa';
say "ok 74 - long list replication in list context: [ @a ]";

@a = ( qw/ryba lufa ryba/ );
$x = grep { $_ eq 'ryba' } @a;
print 'not ' unless $x == 2;
say "ok 75 - simple grep {eq} in scalar context works: [ @a ]";

@a = ( qw/ryba lufa ryba koza/ );
$x = grep { $_ =~ /[bz]a$/ } @a;
print 'not ' unless $x == 3;
say "ok 76 - simple grep {=~} in scalar context works: [ @a ]";

@a = ( qw/ryba lufa ryba/ );
@y = grep { $_ eq 'ryba' } @a;
print 'not ' unless @y == 2 and $y[0] eq 'ryba' and $y[1] eq 'ryba';
say "ok 77 - simple grep {eq} in list context works: [ @a => @y ]";

@a = ( qw/ryb luf ryb/ );
@y = map { $_ . 'a' } @a;
print 'not ' unless @y == 3 and $y[0] eq 'ryba' and $y[1] eq 'lufa';
say "ok 78 - simple map {.} in list context works: [ @a => @y ]";

@a = ( qw/ryb luf ryb/ );
$x = map { $_ . 'a' } @a;
print 'not ' unless $x == 3;
say "ok 79 - simple map {.} in scalar context works: [ @a => @y ]";

@a = ( qw/ryb luf ryb/ );
@y = map { $_ =~ /^ry/ ? $_ . 'a' : () } @a;
print 'not ' unless @y == 2 and $y[0] eq 'ryba' and $y[1] eq 'ryba';
say "ok 80 - map { ? . : () } in list context works: [ @a => @y ]";

@a = ( qw/ryb luf ryb/ );
$x = map { $_ =~ /^ry/ ? $_ . 'a' : () } @a;
print 'not ' unless $x == 2;
say "ok 81 - map { ? . : () } in scalar context works: [ @a => @y ]";

#@a = ( qw/ryba lufa/ );
#@y = map { $_ =~ /^ry/ ? $_, $_ : $_ } @a;
#print 'not ' unless @y == 3 and $y[0] eq $y[1] and $y[0] eq 'ryba';
#say "ok 82 - map { ? ( , ) : () } in list context works: [ @a => @y ]";

@a = qw/ryba lufa koza/;
@y = sort { $a cmp $b } @a;
print 'not ' unless $y[0] eq 'koza' and $y[1] eq 'lufa' and $y[2] eq 'ryba';
say "ok 83 - sort {cmp} works: [ @a => @y ]";

@a = qw/ryba lufa koza/;
@y = sort { $a cmp $b } @a;
print 'not ' unless $a[0] eq 'ryba' and $a[1] eq 'lufa' and $a[2] eq 'koza';
say "ok 84 - sort {cmp} preserves the original array: [ @a => @y ]";

{
    package Just::For::Fun;

    $a = "something";

    @a = qw/ryba lufa koza/;
    @y = sort { $a cmp $b } @a;
    print 'not ' unless $a eq 'something' and $y[0] eq 'koza' and $y[1] eq 'lufa' and $y[2] eq 'ryba';
    say "ok 85 - sort localizes \$a & \$b properly [ @a => @y ]";
}

my $val = '2015-08-28';
$val =~ s/-//g;
print 'not ' unless $val eq '20150828';
say 'ok 86 - search-replace regex works';

my @range = 1..4;
print 'not ' unless ($range[0] == 1 && $range[1] == 2 && $range[2] == 3 && $range[3] == 4 && !defined $range[4]);
say 'ok 87 - range in list context works';

my @yarr = 1..4;
print 'not ' unless (shift @yarr) == 1;
say "ok 88 - array shift: [ @yarr ]";

for (89..90) {
    say "ok $_ - for loop with \$_";
}
for my $x (91..92) {
    say "ok $x - for loop with lexical";
}
for $x_global (93..94) {
    say "ok $x_global - for loop with global";
}
for  (my $x = 95; $x < 97; $x++) {
    say "ok $x - C-style for loop";
}
say "ok $_ - for statement modifier" for 97..98;

@s = qw/ this that /;
@r = reverse @s;
print 'not ' unless $r[0] eq 'that' && $r[1] eq 'this';
say "ok 99 - list reverse [@r]";
$r = reverse @s;
print 'not ' unless $r eq 'tahtsiht';
say "ok 100 - string reverse [$r]";

@a = qw/ryba lufa koza/;
@y = sort @a;
print 'not ' unless $y[0] eq 'koza' and $y[1] eq 'lufa' and $y[2] eq 'ryba';
say "ok 101 - sort {list} works: [ @a => @y ]";

my $coderef = sub { 1; };
print 'not ' unless ref $coderef eq 'CODE';
say "ok 102 - coderef returns proper 'ref': " . ref $coderef;

my $href = {};
bless $href, "Test::More";
print 'not ' unless ref $href eq 'Test::More';
say "ok 103 - blessed hashref returns proper 'ref': " . ref $href;

{
	package Test::Methods;

	sub do_own { return ref $_[0] ? ref $_[0] : $_[0] };
}

$href = {};
my $result;
bless $href, 'Test::Methods';
$result = $href->do_own;
print 'not ' unless $result eq 'Test::Methods';
say "ok 104 - not inherited methods are being called properly on object: $result";

$result = Test::Methods->do_own();
print 'not ' unless $result eq 'Test::Methods';
say "ok 105 - class methods are being called properly: $result";

__END__


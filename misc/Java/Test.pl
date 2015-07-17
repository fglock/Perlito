#
#   $ touch Test.class
#   $ rm Test.class
#   $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava misc/Java/Test.pl > Test.java
#   $ javac Test.java
#   $ java Test
#
#   one liner:
#   $ touch Test.class ; rm Test.class ; perl perlito5.pl -Isrc5/lib -I. -It -Cjava misc/Java/Test.pl > Test.java ; javac Test.java ; java Test
#

print "1..46\n";
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

$s = "[" . @aa . "]";
if ($s ne "[57869]") {
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

package my::Sample { import => "misc.Java.Sample" };

$x = my::Sample->new(); 

if (ref($x) ne 'my::Sample') {
    print "not ";
}
say "ok 35 - java object ref: ", ref($x), " ", $x;

# convert to native
my my::Sample $x_native = $x->to_mySample(); 

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

if (@aa != 3) { print "not" }
say "ok 41 - data structure";
if (keys(%hh) != 2) { print "not" }
say "ok 42 - data structure";

@main::aa = ();
%main::hh = ();

$aa = undef;
$hh = undef;
$aa->[1]{"a"} = 12;
$hh->{"1"}{"a"} = 13;
$aa->[2][1] = 14;
$hh->{"2"}[1] = 15;
if (@$aa != 3) { print "not" }
say "ok 43 - data structure";
if (keys(%$hh) != 2) { print "not" }
say "ok 44 - data structure";

$aa->[1]->{"a"} = 12;
$hh->{"1"}->{"a"} = 13;
$aa->[2]->[1] = 14;
$hh->{"2"}->[1] = 15;
if (@$aa != 3) { print "not" }
say "ok 45 - data structure";
if (keys(%$hh) != 2) { print "not" }
say "ok 46 - data structure";

# TODO - test these
my $a;
# @$a = (1,2,3);
# %$a = (a => 1, b => 2);
# $$a = 5;

# @$main::a = (1,2,3);
# %$main::a = (a => 1, b => 2);
# $$main::a = 5;

# initialize a typed variable by dereferencing a Perl object
# TODO - is it possible to dereference automatically
my my::Sample $z = my::Sample->new()->to_mySample();
# TODO - cast typed variable to Perl object automatically
# say "ok 45 - initialize a typed variable by dereferencing a Perl object: $z\n";

__END__


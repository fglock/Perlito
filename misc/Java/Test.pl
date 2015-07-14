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

print "1..13\n";
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


__END__


pClosure c = new pClosure(s) {
        public pObject apply(int want, pObject... args) {
            System.out.println("called MyClosure with " + this.env.to_string());
            return new pInt(0);
        }
    };
c.apply(pCx.VOID);


// perl "block"
( new pClosure(s) {
        public pObject apply(int want, pObject... args) {
            pCORE.say(pCx.VOID, new pString("STDOUT"), new pArray(new pString("inside block " + this.env.to_string())));
            return new pInt(0);
        }
    }
).apply(pCx.VOID);

new pClosure(s) {
        public pObject apply(int want, pObject... args) {
            pCORE.say(pCx.VOID, new pString("STDOUT"), new pArray(new pString("inside block " + this.env.to_string())));
            // pCORE.say(pCx.VOID, new pString("STDOUT"), new pArray(new pString("inside block " + this.env.to_string() + " "), v_n));
            return new pInt(0);
        }
    }.apply(pCx.VOID);

// print special chars
pCORE.say(pCx.VOID, new pString("STDOUT"), new pArray(new pString("x" + (char)10 + "y")));

pArray aaa = new pArray(new pInt(10), new pInt(20), new pInt(30), aa);
pCORE.say(pCx.VOID, new pString("STDOUT"), new pArray(new pString("array size is "), pCORE.scalar(pCx.SCALAR, aaa)));



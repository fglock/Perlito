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

my $x = 123;
print "1..7\n";
print "ok 1 - print() works\n";
say   "ok 2 - say() works";
$x = "ok 3";
say   "$x - scalar interpolation in string works";
$x = 4;
say   "ok $x - scalar number interpolation in string works";

Java::inline ' System.out.println("ok 5 - Java::inline works"); ';

my @aa = (5,7,8,6,9);
say "ok $aa[3] - array works";

my %hh = ( x => 7, y => 8 );
say "ok $hh{x} - hash works";
# my $h_key = "y";
# say "ok $hh{$h_key} - hash works";

__END__


        pClosure c = new pClosure(s) {
                public pObject apply(int want, pObject... args) {
                    System.out.println("called MyClosure with " + this.env.to_string());
                    return new pInt(0);
                }
            };
        c.apply(pCx.VOID);

        pScalar vv = new pScalar();
        System.out.println("Scalar " + vv.to_string());

        pArray aa = new pArray();
        System.out.println("Array " + aa.to_string());

        aa.aset(i, v_n);
        System.out.println("Array get " + aa.aget(i).to_string());

        pHash hh = new pHash();
        System.out.println("Hash " + hh.to_string());

        hh.hset(i, v_n);
        System.out.println("Hash get " + hh.hget(i).to_string());

        pCORE.print(pCx.VOID, new pString("STDOUT"), new pArray(new pString("HERE\n")));

        pHashRef hr = new pHashRef(hh);
        pCORE.say(pCx.VOID, new pString("STDOUT"), new pArray(new pString("ref is "), pCORE.ref(pCx.SCALAR, new pArray(hr))));

        pScalar v_x = new pScalar(aa);
        pCORE.say(pCx.VOID, new pString("STDOUT"), new pArray(new pString("scalar is "), v_x));


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
    }
}

EOT



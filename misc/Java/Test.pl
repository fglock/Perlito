#
#   $ perl -I src5/lib misc/Java/Test.pl > Test.java
#   $ javac Test.java
#   $ java Test
#

use Perlito5::Java::Runtime;

print Perlito5::Java::Runtime->emit_java();

print <<'EOT';

class Test {
    public static void main(String[] args) {

        pString s = new pString("456");
        pInt i = new pInt(123);
        pNum n = new pNum(123.456);
        pBool t = new pBool(true);
        pBool f = new pBool(false);
        pObject x;

        x = i.add(s);
        x = x.add( new pInt(4) );
        x.the_int_method();
        s.the_int_method();
        System.out.println(x.to_string());
        x = s.add(i);
        System.out.println(x.to_string());
        x = s.add(n);
        System.out.println(x.to_string());
        System.out.println(t.to_string());
        System.out.println(f.to_string());

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

        aa.aset(i, n);
        System.out.println("Array get " + aa.aget(i).to_string());

        pHash hh = new pHash();
        System.out.println("Hash " + hh.to_string());

        hh.hset(i, n);
        System.out.println("Hash get " + hh.hget(i).to_string());

        pCORE.print(pCx.VOID, new pString("HERE\n"));

        pHashRef hr = new pHashRef(hh);
        pCORE.say(pCx.VOID, new pString("ref is "), pCORE.ref(pCx.SCALAR, hr));

        pScalar v_x = new pScalar(aa);
        pCORE.say(pCx.VOID, new pString("scalar is "), v_x);
    }
}

EOT



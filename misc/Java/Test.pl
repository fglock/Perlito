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

        PerlitoString s = new PerlitoString("456");
        PerlitoInt i = new PerlitoInt(123);
        PerlitoNum n = new PerlitoNum(123.456);
        PerlitoBool t = new PerlitoBool(true);
        PerlitoBool f = new PerlitoBool(false);
        PerlitoObject x;

        x = i.add(s);
        x = x.add( new PerlitoInt(4) );
        x.the_int_method();
        s.the_int_method();
        System.out.println(x.to_string());
        x = s.add(i);
        System.out.println(x.to_string());
        x = s.add(n);
        System.out.println(x.to_string());
        System.out.println(t.to_string());
        System.out.println(f.to_string());

        PerlitoClosure c = new PerlitoClosure(s) {
                public PerlitoObject apply() {
                    System.out.println("called MyClosure with " + this.env.to_string());
                    return new PerlitoInt(0);
                }
            };
        c.apply();

        PerlitoHash h = new PerlitoHash();
        System.out.println(h.to_string());
    }
}

EOT



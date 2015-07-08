use v5;

package Perlito5::Java::Runtime;

sub emit_java {

    return <<'EOT';
/*
    lib/Perlito5/Java/Runtime.pm
*/

import java.util.ArrayList;
import java.util.HashMap;

class pContext {
    public static final int VOID   = 0;
    public static final int SCALAR = 1;
    public static final int LIST   = 2;
}
class pCORE {
    public static final pObject print(int want, pObject... args) {
        for(pObject s : args)
        {
            System.out.print(s.to_string());
        }
        return new pInt(1);
    }
    public static final pObject say(int want, pObject... args) {
        for(pObject s : args)
        {
            System.out.print(s.to_string());
        }
        System.out.println("\n");
        return new pInt(1);
    }
}
class pObject {
    public pObject() {
    }
    public String to_string() {
        System.out.println("error .to_string!");
        return "";
    }
    public int to_int() {
        System.out.println("error .to_int!");
        return 0;
    }
    public double to_num() {
        System.out.println("error .to_num!");
        return 0.0;
    }
    public boolean to_bool() {
        System.out.println("error .to_bool!");
        return true;
    }
    public pObject add(pObject s) {
        return this.to_num_or_int().add(s);
    }
    public boolean is_int() {
        return false;
    }
    public boolean is_num() {
        return false;
    }
    public boolean is_string() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public pObject to_num_or_int() {
        return new pInt(0);
    }
    public void the_int_method() {
        System.out.println("error!");
    }
}
class pClosure extends pObject {
    public pObject env;
    public pClosure(pObject env) {
        this.env = env;
    }
    public pObject apply(int want, pObject... args) {
        System.out.println("error!");
        return new pInt(0);
    }
}
class pScalar extends pObject {
    private pObject o;
    // Note: 3 versions of pScalar()
    public pObject pScalar() {
        return this;
    }
    public pObject pScalar(pObject o) {
        this.o = o;
        return this;
    }
    public pObject pScalar(pScalar o) {
        this.o = o.get();
        return this;
    }

    public pObject get() {
        return this.o;
    }

    // Note: 2 versions of set()
    public pObject set(pObject o) {
        this.o = o;
        return this;
    }
    public pObject set(pScalar o) {
        this.o = o.get();
        return this;
    }

    public String to_string() {
        if (this.o == null) {
            return "";
        }
        return this.o.to_string();
    }
    public int to_int() {
        if (this.o == null) {
            return 0;
        }
        return this.o.to_int();
    }
    public double to_num() {
        if (this.o == null) {
            return 0.0;
        }
        return this.o.to_num();
    }
    public boolean to_bool() {
        if (this.o == null) {
            return false;
        }
        return this.o.to_bool();
    }
    public pObject add(pObject s) {
        if (this.o == null) {
            this.o = new pInt(0);
        }
        return this.o.add(s);
    }
    public boolean is_int() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_int();
    }
    public boolean is_num() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_num();
    }
    public boolean is_string() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_string();
    }
    public boolean is_bool() {
        if (this.o == null) {
            return false;
        }
        return this.o.is_bool();
    }
    public pObject to_num_or_int() {
        if (this.o == null) {
            this.o = new pUndef();
        }
        return this.o.to_num_or_int();
    }
}
class pArray extends pObject {
    private ArrayList<pObject> a;
    public pArray() {
        this.a = new ArrayList<pObject>();
    }
    public pObject aget(pObject i) {
        int pos  = i.to_int();
        if (pos > this.a.size()) {
            return new pUndef();
        }
        return this.a.get(i.to_int());
    }

    // Note: 2 versions of set()
    public pObject aset(pObject i, pObject v) {
        int size = this.a.size();
        int pos  = i.to_int();
        while (size < pos) {
            this.a.add( new pUndef() );
            size++;
        }
        this.a.add(i.to_int(), v);
        return v;
    }
    public pObject aset(pObject i, pScalar v) {
        int size = this.a.size();
        int pos  = i.to_int();
        while (size < pos) {
            this.a.add( new pUndef() );
            size++;
        }
        this.a.add(i.to_int(), v.get());
        return v;
    }

    public String to_string() {
        // TODO
        return "" + this.hashCode();
    }
    public int to_int() {
        return this.a.size();
    }
    public double to_num() {
        return 0.0 + this.to_int();
    }
    public boolean to_bool() {
        return (this.a.size() > 0);
    }
    public pObject add(pObject s) {
        return this.to_num_or_int().add(s);
    }
    public boolean is_int() {
        return false;
    }
    public boolean is_num() {
        return false;
    }
    public boolean is_string() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public pObject to_num_or_int() {
        return new pInt(this.to_int());
    }
}
class pHash extends pObject {
    private HashMap<String, pObject> h;
    public pHash() {
        this.h = new HashMap<String, pObject>();
    }
    public pObject hget(pObject i) {
        pObject o = this.h.get(i.to_string());
        if (o == null) {
            return new pUndef();
        }
        return o;
    }

    // Note: 2 versions of set()
    public pObject hset(pObject i, pObject v) {
        this.h.put(i.to_string(), v);
        return v;
    }
    public pObject hset(pObject i, pScalar v) {
        this.h.put(i.to_string(), v.get());
        return v;
    }

    public String to_string() {
        // TODO
        return "" + this.hashCode();
    }
    public int to_int() {
        // TODO
        return this.hashCode();
    }
    public double to_num() {
        return 0.0 + this.to_int();
    }
    public boolean to_bool() {
        return true;
    }
    public pObject add(pObject s) {
        return this.to_num_or_int().add(s);
    }
    public boolean is_int() {
        return false;
    }
    public boolean is_num() {
        return false;
    }
    public boolean is_string() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public pObject to_num_or_int() {
        return new pInt(this.to_int());
    }
}
class pUndef extends pObject {
    public pUndef() {
    }
    public int to_int() {
        return 0;
    }
    public double to_num() {
        return 0.0;
    }
    public String to_string() {
        return "";
    }
    public boolean to_bool() {
        return false;
    }
    public boolean is_bool() {
        return false;
    }
    public pObject to_num_or_int() {
        return new pInt(0);
    }
}
class pBool extends pObject {
    private boolean i;
    public pBool(boolean i) {
        this.i = i;
    }
    public int to_int() {
        if (this.i) {
            return 1;
        }
        else {
            return 0;
        }
    }
    public double to_num() {
        if (this.i) {
            return 1.0;
        }
        else {
            return 0.0;
        }
    }
    public String to_string() {
        if (this.i) {
            return "true";
        }
        else {
            return "false";
        }
    }
    public boolean to_bool() {
        return this.i;
    }
    public boolean is_bool() {
        return true;
    }
    public pObject to_num_or_int() {
        return new pInt(this.to_int());
    }
}
class pInt extends pObject {
    private int i;
    public pInt(int i) {
        this.i = i;
    }
    public int to_int() {
        return this.i;
    }
    public double to_num() {
        return (double)(this.i);
    }
    public String to_string() {
        return "" + this.i;
    }
    public boolean to_bool() {
        return this.i != 0;
    }
    public boolean is_int() {
        return true;
    }
    public void the_int_method() {
        System.out.println("Here!");
    }
    public pObject add(pObject s) {
        System.out.println("Int.add Object!");
        if (s.is_int()) {
            return new pInt( this.i + s.to_int() );
        }
        return s.to_num_or_int().add(this);
    }
    public pObject to_num_or_int() {
        return this;
    }
}
class pNum extends pObject {
    private double i;
    public pNum(double i) {
        this.i = i;
    }
    public int to_int() {
        return (int)(this.i);
    }
    public double to_num() {
        return this.i;
    }
    public String to_string() {
        return "" + this.i;
    }
    public boolean to_bool() {
        return this.i != 0.0;
    }
    public pNum add(pObject s) {
        return new pNum( this.i + s.to_num() );
    }
    public boolean is_num() {
        return true;
    }
    public pObject to_num_or_int() {
        return this;
    }
}
class pString extends pObject {
    private java.lang.String s;
    public pString(String s) {
        this.s = s;
    }
    public int to_int() {
        return Integer.parseInt(this.s.trim());
    }
    public double to_num() {
        return Double.parseDouble(this.s.trim());
    }
    public String to_string() {
        return this.s;
    }
    public boolean to_bool() {
        return this.s != ""
            && this.s != "0";
    }
    public boolean is_string() {
        return true;
    }
    public pObject to_num_or_int() {
        if (this.s.indexOf('.') > 0) {
            try {
                return new pNum(this.to_num());
            } catch (NumberFormatException nfe) {
                return new pInt(0);
            }
        }
        try {
            return new pInt(this.to_int());
        } catch (NumberFormatException nfe) {
            return new pInt(0);
        }
    } 
}

EOT

} # end of emit_javascript2()

1;

__END__

class HelloWorldApp {
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
                public pObject apply() {
                    System.out.println("called MyClosure with " + this.env.to_string());
                    return new pInt(0);
                }
            };
        c.apply();

        pHash h = new pHash();
        System.out.println(h.to_string());
    }
}


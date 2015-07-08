use v5;

package Perlito5::Java::Runtime;

sub emit_java {

    return <<'EOT';
/*
    lib/Perlito5/Java/Runtime.pm
*/

import java.util.ArrayList;
import java.util.HashMap;

class PerlitoObject {
    public PerlitoObject() {
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
    public PerlitoObject add(PerlitoObject s) {
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
    public PerlitoObject to_num_or_int() {
        return new PerlitoInt(0);
    }
    public void the_int_method() {
        System.out.println("error!");
    }
}
class PerlitoClosure extends PerlitoObject {
    public PerlitoObject env;
    public PerlitoClosure(PerlitoObject env) {
        this.env = env;
    }
    public PerlitoObject apply() {
        System.out.println("error!");
        return new PerlitoInt(0);
    }
}
class PerlitoScalar extends PerlitoObject {
    private PerlitoObject o;
    public PerlitoScalar() {
        this.o = new PerlitoUndef();
    }
    public String to_string() {
        return this.o.to_string();
    }
}
class PerlitoArray extends PerlitoObject {
    private ArrayList<PerlitoObject> a;
    public PerlitoArray() {
        this.a = new ArrayList<PerlitoObject>();
    }
    public PerlitoObject aget(PerlitoObject i) {
        return this.a.get(i.to_int());
    }
    public PerlitoObject aset(PerlitoObject i, PerlitoObject v) {
        int size = this.a.size();
        int pos  = i.to_int();
        while (size < pos) {
            this.a.add( new PerlitoUndef() );
            size++;
        }
        this.a.add(i.to_int(), v);
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
    public PerlitoObject add(PerlitoObject s) {
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
    public PerlitoObject to_num_or_int() {
        return new PerlitoInt(this.to_int());
    }

}
class PerlitoHash extends PerlitoObject {
    private HashMap<String, PerlitoObject> h;
    public PerlitoHash() {
        this.h = new HashMap<String, PerlitoObject>();
    }
    public PerlitoObject hget(PerlitoObject i) {
        return this.h.get(i.to_string());
    }
    public PerlitoObject hset(PerlitoObject i, PerlitoObject v) {
        this.h.put(i.to_string(), v);
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
    public PerlitoObject add(PerlitoObject s) {
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
    public PerlitoObject to_num_or_int() {
        return new PerlitoInt(this.to_int());
    }
}
class PerlitoUndef extends PerlitoObject {
    public PerlitoUndef() {
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
    public PerlitoObject to_num_or_int() {
        return new PerlitoInt(0);
    }
}
class PerlitoBool extends PerlitoObject {
    private boolean i;
    public PerlitoBool(boolean i) {
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
    public PerlitoObject to_num_or_int() {
        return new PerlitoInt(this.to_int());
    }
}
class PerlitoInt extends PerlitoObject {
    private int i;
    public PerlitoInt(int i) {
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
    public PerlitoObject add(PerlitoObject s) {
        System.out.println("Int.add Object!");
        if (s.is_int()) {
            return new PerlitoInt( this.i + s.to_int() );
        }
        return s.to_num_or_int().add(this);
    }
    public PerlitoObject to_num_or_int() {
        return this;
    }
}
class PerlitoNum extends PerlitoObject {
    private double i;
    public PerlitoNum(double i) {
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
    public PerlitoNum add(PerlitoObject s) {
        return new PerlitoNum( this.i + s.to_num() );
    }
    public boolean is_num() {
        return true;
    }
    public PerlitoObject to_num_or_int() {
        return this;
    }
}
class PerlitoString extends PerlitoObject {
    private java.lang.String s;
    public PerlitoString(String s) {
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
    public PerlitoObject to_num_or_int() {
        if (this.s.indexOf('.') > 0) {
            try {
                return new PerlitoNum(this.to_num());
            } catch (NumberFormatException nfe) {
                return new PerlitoInt(0);
            }
        }
        try {
            return new PerlitoInt(this.to_int());
        } catch (NumberFormatException nfe) {
            return new PerlitoInt(0);
        }
    } 
}

EOT

} # end of emit_javascript2()

1;

__END__

class HelloWorldApp {
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


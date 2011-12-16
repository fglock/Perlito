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
    public PerlitoObject apply(PerlitoObject param) {
        System.out.println("error!");
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
class PerlitoHash extends PerlitoObject {
    private java.util.HashMap h;
    public PerlitoHash() {
        this.h = new java.util.HashMap();
    }
    // h.put("this", "x");
}
class PerlitoArray extends PerlitoObject {
    private java.util.ArrayList a;
    public PerlitoArray() {
        this.a = new java.util.ArrayList();
    }
    // a.set(10, "x");
}


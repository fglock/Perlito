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
    public PerlitoObject to_num_or_int() {
        return new PerlitoInt(0);
    }
    public void the_int_method() {
        System.out.println("error!");
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
    public boolean is_int() {
        return true;
    }
    public void the_int_method() {
        System.out.println("Here!");
    }
    public PerlitoObject add(PerlitoObject s) {
        System.out.println("Int.add Object!");
        if (s.is_int()) {
            return new PerlitoInt( this.to_int() + s.to_int() );
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
    public PerlitoNum add(PerlitoObject s) {
        return new PerlitoNum( this.to_num() + s.to_num() );
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
        return Integer.parseInt(this.s);
    }
    public double to_num() {
        return Double.parseDouble(this.s);
    }
    public String to_string() {
        return this.s;
    }
    public boolean is_string() {
        return true;
    }
    public PerlitoObject to_num_or_int() {
        if (this.s.indexOf('.') > 0) {
            double d = 0.0;
            try {
                d = Double.valueOf(this.s.trim()).doubleValue();
            } catch (NumberFormatException nfe) {
                return new PerlitoInt(0);
            }
            return new PerlitoNum(d);
        }
        int i = 0;
        try {
            i = Integer.parseInt(this.s);
        } catch (NumberFormatException nfe) {
            return new PerlitoInt(0);
        }
        return new PerlitoInt(i);
    } 
}
class HelloWorldApp {
    public static void main(String[] args) {

        PerlitoString s = new PerlitoString("456");
        PerlitoInt i = new PerlitoInt(123);
        PerlitoNum n = new PerlitoNum(123.456);
        PerlitoObject x;

        x = i.add(s);
        x = x.add( new PerlitoInt(4) );
        x.the_int_method();
        s.the_int_method();
        System.out.println(x.to_string()); // Display the string.
        x = s.add(i);
        System.out.println(x.to_string()); // Display the string.
        x = s.add(n);
        System.out.println(x.to_string()); // Display the string.
    }
}


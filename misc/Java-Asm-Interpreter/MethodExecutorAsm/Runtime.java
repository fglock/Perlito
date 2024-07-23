public class Runtime {
    private final long i; 
    public Runtime(long i) {
        this.i = i;
    }
    public Runtime(int i) {
        this.i = (long)i;
    }

    public String toString() {
        return String.valueOf(this.i);
    }

    public static Runtime print(int a) {
        System.out.println("value=" + a);
        return new Runtime(a);
    }

    public static Runtime print(Object a) {
        return Runtime.print( (Runtime)a );
    }

    public static Runtime print(Runtime a) {
        return a.print();
    }

    public Runtime print() {
        System.out.println("value=" + this.i);
        return this;
    }

    public static Runtime make(int a) {
        return new Runtime(a);
    }

    public Runtime add(int a, int b) {
        return new Runtime(a + b);
    }

    public Runtime add(int b) {
        return new Runtime(this.i + b);
    }

    public Runtime add(Runtime b) {
        return new Runtime(this.i + b.i);
    }

    public Runtime multiply(int b) {
        return new Runtime(this.i * b);
    }

    public Runtime multiply(Runtime b) {
        return new Runtime(this.i * b.i);
    }
}

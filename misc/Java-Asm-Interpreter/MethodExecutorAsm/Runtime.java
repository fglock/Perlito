import java.lang.reflect.Constructor;
import java.util.concurrent.Callable;
import java.util.HashMap;

public class Runtime {
    private final long i; 

    public static HashMap<String, Class<?>> anonSubs = new HashMap<String, Class<?>>();    

    Constructor<? extends Callable<?>> subroutineReference;

    public Runtime(long i) {
        this.i = i;
    }
    public Runtime(int i) {
        this.i = (long)i;
    }

    @SuppressWarnings("unchecked")
    // public Runtime( Constructor<? extends Callable<Runtime>> subroutineReference ) {
    public Runtime( Constructor<? extends Callable<?>> subroutineReference ) {
        this.i = 0;
        this.subroutineReference = subroutineReference;
    }

    public static Runtime make_sub( String className ) throws Exception {

        // XXX TODO - the class can be removed from the hash once we retrieve the constructor
        Class<?> clazz = Runtime.anonSubs.get(className);
        System.out.println("make_sub " + className);

        @SuppressWarnings("unchecked")
        Constructor<? extends Callable<?>> constructor = (Constructor<? extends Callable<?>>) clazz.getConstructor(Runtime.class);

        Runtime rr = new Runtime(-1);
        rr.subroutineReference = constructor;
        return rr;
    }

    public Runtime apply(Runtime a) throws Exception {
        Runtime result = (Runtime) subroutineReference.newInstance(a).call();
        return result;
    }

    public String toString() {
        return String.valueOf(this.i);
    }

    public static boolean is_false() {
        return false;
    }

    public static boolean is_true() {
        return true;
    }

    public static Runtime print(String a) {
        System.out.println("value=" + a);
        return new Runtime(0);
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

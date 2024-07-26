import java.lang.reflect.Constructor;
import java.util.concurrent.Callable;
import java.util.HashMap;

public class Runtime {
    private final long i; 
    Constructor<? extends Callable<?>> subroutineReference;     // used by apply()

    public static HashMap<String, Class<?>> anonSubs = new HashMap<String, Class<?>>();    // temp storage for make_sub()

    public Runtime(long i) {
        this.i = i;
    }
    public Runtime(int i) {
        this.i = (long)i;
    }

    public static Runtime make_sub( String className ) throws Exception {
        Class<?> clazz = Runtime.anonSubs.remove(className);

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

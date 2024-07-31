import java.lang.reflect.Method;
import java.util.HashMap;

public class Runtime {
  long i;
  String s;
  Method subroutineReference; // used by apply()

  public static HashMap<String, Class<?>> anonSubs =
      new HashMap<String, Class<?>>(); // temp storage for make_sub()

  public Runtime() {
    this.i = 0;
  }

  public Runtime(long i) {
    this.i = i;
  }

  public Runtime(int i) {
    this.i = (long) i;
  }

  public Runtime(String s) {
    this.s = s;
  }

  public static Runtime make_sub(String className) throws Exception {
    // finish setting up a CODE object
    Class<?> clazz = Runtime.anonSubs.remove(className);
    Method mm = clazz.getMethod("apply", Runtime.class, ContextType.class);
    Runtime rr = new Runtime(-1);
    rr.subroutineReference = mm;
    return rr;
  }

  public Runtime apply(Runtime a, ContextType callContext) throws Exception {
    Runtime result = (Runtime) subroutineReference.invoke(null, a, callContext);
    return result;
  }

  public Runtime set(Runtime a) {
    this.i = a.i;
    this.s = a.s;
    this.subroutineReference = a.subroutineReference;
    return this;
  }

  public String toString() {
    return String.valueOf(this.i);
  }

  public boolean toBoolean() {
    return this.i != 0;
  }

  public static boolean is_false() {
    return false;
  }

  public static boolean is_true() {
    return true;
  }

  public static Runtime print(String a) {
    System.out.println("value=" + a);
    return new Runtime(1);
  }

  public static Runtime print(int a) {
    System.out.println("value=" + a);
    return new Runtime(1);
  }

  public static Runtime print(Object a) {
    Runtime.print((Runtime) a);
    return new Runtime(1);
  }

  public static Runtime print(Runtime a) {
    a.print();
    return new Runtime(1);
  }

  public Runtime print() {
    System.out.println("value=" + this.i + " " + this.s);
    return new Runtime(1);
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

  public Runtime subtract(Runtime b) {
    return new Runtime(this.i - b.i);
  }

  public Runtime multiply(int b) {
    return new Runtime(this.i * b);
  }

  public Runtime multiply(Runtime b) {
    return new Runtime(this.i * b.i);
  }

  public Runtime divide(Runtime b) {
    return new Runtime(this.i / b.i);
  }
}

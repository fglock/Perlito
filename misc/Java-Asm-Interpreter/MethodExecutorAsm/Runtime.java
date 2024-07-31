import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;

public class Runtime {
  long i;
  String s;
  Method subroutineReference; // used by apply()

  public static HashMap<String, Class<?>> anonSubs =
      new HashMap<String, Class<?>>(); // temp storage for make_sub()
  public static HashMap<String, EmitterContext> evalContext =
      new HashMap<String, EmitterContext>(); // storage for eval string compiler context

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

  // TODO eval string
  public static Runtime eval_string(Runtime code, String evalTag) throws Exception {
    // retrieve the context that was saved at compile-time
    EmitterContext ctx = Runtime.evalContext.get(evalTag);

    // Create the Token list
    Lexer lexer = new Lexer(code.toString());
    List<Token> tokens = lexer.tokenize(); // Tokenize the Perl code
    // Create the AST
    // Create an instance of ErrorMessageUtil with the file name and token list
    ErrorMessageUtil errorUtil = new ErrorMessageUtil(ctx.fileName, tokens);
    Parser parser = new Parser(errorUtil, tokens); // Parse the tokens
    Node ast = parser.parse(); // Generate the abstract syntax tree (AST)
    System.out.println("eval_string AST:\n" + ast + "--\n");
    return code;    // XXX
  }

  public Runtime set(Runtime a) {
    this.i = a.i;
    this.s = a.s;
    this.subroutineReference = a.subroutineReference;
    return this;
  }

  public String toString() {
    if (this.s != null) {
        return this.s;
    }
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

  public Runtime stringConcat(Runtime b) {
    return new Runtime(this.toString() + b.toString());
  }
}

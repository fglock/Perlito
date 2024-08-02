import java.lang.reflect.Method;
import java.util.*;

public class Runtime {
    private enum Type {
        INTEGER, DOUBLE, STRING, CODE, UNDEF, REFERENCE
        // also BLESSED and special literals like filehandles, typeglobs, and regular expressions
    }

    // TODO add cache for integer/string values
    public Type type;
    public Object value;

    public static HashMap<String, Class<?>> anonSubs =
        new HashMap<String, Class<?>>(); // temp storage for make_sub()
    public static HashMap<String, EmitterContext> evalContext =
        new HashMap<String, EmitterContext>(); // storage for eval string compiler context

    // Constructors
    public Runtime() {
        this.type = Type.UNDEF;
        this.value = value;
    }

    public Runtime(long value) {
        this.type = Type.INTEGER;
        this.value = value;
    }

    public Runtime(int value) {
        this.type = Type.INTEGER;
        this.value = (long) value;
    }

    public Runtime(double value) {
        this.type = Type.DOUBLE;
        this.value = value;
    }

    public Runtime(String value) {
        this.type = Type.STRING;
        this.value = value;
    }

    public Runtime(Runtime value) {
        this.type = Type.REFERENCE;
        this.value = value;
    }

    public Runtime(Method value) {
        this.type = Type.CODE;
        this.value = value;
    }

    // Getters
    public long getLong() {
        switch (type) {
            case INTEGER:
                return (long) value;
            case DOUBLE:
                return (long) ((double) value);
            case STRING:
                try {
                    return Long.parseLong((String) value);
                } catch (NumberFormatException e) {
                    return 0;
                }
            case CODE:
                return ((Method) this.value).hashCode(); // Use Method's hashCode as the ID
            default:
                return 0;
        }
    }

    private double getDouble() {
        switch (this.type) {
            case INTEGER:
                return (long) this.value;
            case DOUBLE:
                return (double) this.value;
            case STRING:
                try {
                    return Double.parseDouble((String) this.value);
                } catch (NumberFormatException e) {
                    return 0.0; // Return zero if the string cannot be converted
                }
            case CODE:
                return ((Method) this.value).hashCode(); // Use Method's hashCode as the ID
            default:
                return 0.0;
        }
    }

    public boolean getBoolean() {
        switch (type) {
            case INTEGER:
                return (long) value != 0;
            case DOUBLE:
                return (long) ((double) value) != 0;
            case STRING:
                String s = (String) value;
                return !s.equals("") && !s.equals("0");
            default:
                return true;
        }
    }

    public String getString() {
        if (type == Type.STRING) {
            return (String) value;
        } else {
            throw new IllegalStateException("Variable does not contain a string");
        }
    }

    // Setters
    public Runtime set(Runtime value) {
        this.type = value.type;
        this.value = value.value;
        return this;
    }

    public Runtime set(long value) {
        this.type = Type.INTEGER;
        this.value = value;
        return this;
    }

    public Runtime set(String value) {
        this.type = Type.STRING;
        this.value = value;
        return this;
    }

    @Override
    public String toString() {
        switch (type) {
            case INTEGER:
                return Long.toString((long) value);
            case DOUBLE:
                return Double.toString((double) value);
            case STRING:
                return (String) value;
            case REFERENCE:
                return "Reference to: " + value.toString();
            case CODE:
                return "Code Reference: " + value.toString();
            default:
                return "Undefined";
        }
    }

  public static Runtime make_sub(String className) throws Exception {
    // finish setting up a CODE object
    Class<?> clazz = Runtime.anonSubs.remove(className);
    Method mm = clazz.getMethod("apply", Runtime.class, ContextType.class);
    return new Runtime(mm);
  }

  public Runtime apply(Runtime a, ContextType callContext) throws Exception {
    if (type == Type.CODE) {
        return (Runtime) ((Method) value).invoke(null, a, callContext);
    } else {
        throw new IllegalStateException("Variable does not contain a code reference");
    }
  }

  public static void eval_string(Runtime code, String evalTag) throws Exception {
    // retrieve the eval context that was saved at program compile-time
    EmitterContext evalCtx = Runtime.evalContext.get(evalTag);

    // TODO - this can be cached for performance
    // retrieve closure variable list
    // alternately, scan the AST for variables and capture only the ones that are used
    Map<Integer, String> visibleVariables = evalCtx.symbolTable.getAllVisibleVariables();
    String[] newEnv = new String[visibleVariables.size()];
    for (Integer index : visibleVariables.keySet()) {
      String variableName = visibleVariables.get(index);
      newEnv[index] = variableName;
    }

    // Process the string source code to create the Token list
    Lexer lexer = new Lexer(code.toString());
    List<Token> tokens = lexer.tokenize(); // Tokenize the Perl code
    // Create the AST
    // Create an instance of ErrorMessageUtil with the file name and token list
    ErrorMessageUtil errorUtil = new ErrorMessageUtil(evalCtx.fileName, tokens);
    Parser parser = new Parser(errorUtil, tokens); // Parse the tokens
    Node ast = parser.parse(); // Generate the abstract syntax tree (AST)

    evalCtx.errorUtil = new ErrorMessageUtil(evalCtx.fileName, tokens);
    Class<?> generatedClass = ASMMethodCreator.createClassWithMethod(
            evalCtx,
            newEnv, // Closure variables
            ast
    );
    return;
  }

  public Runtime print() {
    System.out.println(this.toString());
    return new Runtime(1);
  }

  public Runtime stringConcat(Runtime b) {
    return new Runtime(this.toString() + b.toString());
  }

    public Runtime add(Runtime arg) {
        if (this.type == Type.STRING || arg.type == Type.STRING) {
            // Try to parse both values as long
            try {
                long val1 = Long.parseLong((String) this.value);
                long val2 = Long.parseLong((String) arg.value);
                return new Runtime(val1 + val2);
            } catch (NumberFormatException e) {
                // If parsing fails, fall back to double addition
                double val1 = this.getDouble();
                double val2 = arg.getDouble();
                return new Runtime(val1 + val2);
            }
        } else if (this.type == Type.DOUBLE || arg.type == Type.DOUBLE) {
            double val1 = this.getDouble();
            double val2 = arg.getDouble();
            return new Runtime(val1 + val2);
        } else {
            long val1 = this.getLong();
            long val2 = arg.getLong();
            return new Runtime(val1 + val2);
        }
    }
}

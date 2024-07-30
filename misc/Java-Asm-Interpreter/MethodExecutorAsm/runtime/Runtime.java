public class Runtime {
    private enum Type {
        INTEGER, DOUBLE, STRING, REFERENCE, CODE, UNDEF, BLESSED
        // also special literals like filehandles, typeglobs, and regular expressions
    }

    public Type type;
    public Object value;

    // Constructors
    public Runtime(int value) {
        this.type = Type.INTEGER;
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

    public Runtime(CodeReference value) {
        this.type = Type.CODE;
        this.value = value;
    }

    // Getters
    public int getInteger() {
        if (type == Type.INTEGER) {
            return (int) value;
        } else {
            throw new IllegalStateException("Variable does not contain an integer");
        }
    }

    public String getString() {
        if (type == Type.STRING) {
            return (String) value;
        } else {
            throw new IllegalStateException("Variable does not contain a string");
        }
    }

    public Runtime getReference() {
        if (type == Type.REFERENCE) {
            return (Runtime) value;
        } else {
            throw new IllegalStateException("Variable does not contain a reference");
        }
    }

    public CodeReference getCode() {
        if (type == Type.CODE_REFERENCE) {
            return (CodeReference) value;
        } else {
            throw new IllegalStateException("Variable does not contain a code reference");
        }
    }

    // Setters
    public void set(Runtime value) {
        this.type = value.type;
        this.value = value.value;
    }

    public void setInteger(int value) {
        this.type = Type.INTEGER;
        this.value = value;
    }

    public void setString(String value) {
        this.type = Type.STRING;
        this.value = value;
    }

    public void setReference(Runtime value) {
        this.type = Type.REFERENCE;
        this.value = value;
    }

    public void setCode(Code value) {
        this.type = Type.CODE;
        this.value = value;
    }

    @Override
    public String toString() {
        switch (type) {
            case INTEGER:
                return Integer.toString((int) value);
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

    public static void main(String[] args) {
        Runtime intVar = new Runtime(42);
        Runtime strVar = new Runtime("Hello, Perl!");
        Runtime refVar = new Runtime(intVar);
        Runtime codeVar = new Runtime((CodeReference) () -> System.out.println("Executing code reference!"));

        System.out.println(intVar);
        System.out.println(strVar);
        System.out.println(refVar);
        System.out.println(codeVar);

        codeVar.getCodeReference().execute();
    }
}

// Example of a code reference interface
@FunctionalInterface
interface CodeReference {
    void execute();
}


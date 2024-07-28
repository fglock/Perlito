public class PerlVariable {
    private enum Type {
        INTEGER, STRING, REFERENCE, CODE_REFERENCE, OBJECT
    }

    public Type type;
    public Object value;

    // Constructors
    public PerlVariable(int value) {
        this.type = Type.INTEGER;
        this.value = value;
    }

    public PerlVariable(String value) {
        this.type = Type.STRING;
        this.value = value;
    }

    public PerlVariable(PerlVariable value) {
        this.type = Type.REFERENCE;
        this.value = value;
    }

    public PerlVariable(CodeReference value) {
        this.type = Type.CODE_REFERENCE;
        this.value = value;
    }

    public PerlVariable(Object value) {
        this.type = Type.OBJECT;
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

    public PerlVariable getReference() {
        if (type == Type.REFERENCE) {
            return (PerlVariable) value;
        } else {
            throw new IllegalStateException("Variable does not contain a reference");
        }
    }

    public CodeReference getCodeReference() {
        if (type == Type.CODE_REFERENCE) {
            return (CodeReference) value;
        } else {
            throw new IllegalStateException("Variable does not contain a code reference");
        }
    }

    public Object getObject() {
        if (type == Type.OBJECT) {
            return value;
        } else {
            throw new IllegalStateException("Variable does not contain an object");
        }
    }

    // Setters
    public void set(PerlVariable value) {
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

    public void setReference(PerlVariable value) {
        this.type = Type.REFERENCE;
        this.value = value;
    }

    public void setCodeReference(CodeReference value) {
        this.type = Type.CODE_REFERENCE;
        this.value = value;
    }

    public void setObject(Object value) {
        this.type = Type.OBJECT;
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
            case CODE_REFERENCE:
                return "Code Reference: " + value.toString();
            case OBJECT:
                return "Object of type: " + value.getClass().getName();
            default:
                return "Undefined";
        }
    }

    public static void main(String[] args) {
        PerlVariable intVar = new PerlVariable(42);
        PerlVariable strVar = new PerlVariable("Hello, Perl!");
        PerlVariable refVar = new PerlVariable(intVar);
        PerlVariable codeVar = new PerlVariable((CodeReference) () -> System.out.println("Executing code reference!"));
        PerlVariable objVar = new PerlVariable(new CustomObject("MyObject"));

        System.out.println(intVar);
        System.out.println(strVar);
        System.out.println(refVar);
        System.out.println(codeVar);
        System.out.println(objVar);

        codeVar.getCodeReference().execute();
    }
}

// Example of a code reference interface
@FunctionalInterface
interface CodeReference {
    void execute();
}

// Example of a custom object
class CustomObject {
    private String name;

    public CustomObject(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "CustomObject{name='" + name + "'}";
    }
}

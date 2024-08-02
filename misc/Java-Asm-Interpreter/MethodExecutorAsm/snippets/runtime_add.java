import java.lang.reflect.Method;

public class Runtime {
    public enum Type {
        INTEGER, DOUBLE, STRING, CODE, // Add other types as needed
    }

    private Type type;
    private Object value;

    // Constructor
    public Runtime(Type type, Object value) {
        this.type = type;
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
                    return 0L; // Return zero if the string cannot be converted
                }
            case CODE:
                return ((Method) value).hashCode(); // Use Method's hashCode as the ID
            default:
                throw new IllegalStateException("Variable does not contain a type that can be converted to a long");
        }
    }

    // Add method
    public Runtime add(Runtime arg) {
        if (this.type == Type.STRING || arg.type == Type.STRING) {
            return addString(this, arg);
        } else if (this.type == Type.DOUBLE || arg.type == Type.DOUBLE) {
            return addDouble(this, arg);
        } else if (this.type == Type.INTEGER || arg.type == Type.INTEGER) {
            return addInteger(this, arg);
        } else if (this.type == Type.CODE || arg.type == Type.CODE) {
            return addCode(this, arg);
        } else {
            throw new IllegalStateException("Unsupported variable type for addition");
        }
    }

    // Helper methods for addition
    private Runtime addString(Runtime var1, Runtime var2) {
        double val1 = toDouble(var1);
        double val2 = toDouble(var2);
        return new Runtime(Type.DOUBLE, val1 + val2);
    }

    private Runtime addDouble(Runtime var1, Runtime var2) {
        double val1 = toDouble(var1);
        double val2 = toDouble(var2);
        return new Runtime(Type.DOUBLE, val1 + val2);
    }

    private Runtime addInteger(Runtime var1, Runtime var2) {
        long val1 = toLong(var1);
        long val2 = toLong(var2);
        return new Runtime(Type.INTEGER, val1 + val2);
    }

    private Runtime addCode(Runtime var1, Runtime var2) {
        long val1 = toLong(var1);
        long val2 = toLong(var2);
        return new Runtime(Type.INTEGER, val1 + val2);
    }

    // Helper method to convert Runtime to double
    private double toDouble(Runtime var) {
        switch (var.type) {
            case INTEGER:
                return (long) var.value;
            case DOUBLE:
                return (double) var.value;
            case STRING:
                try {
                    return Double.parseDouble((String) var.value);
                } catch (NumberFormatException e) {
                    return 0.0; // Return zero if the string cannot be converted
                }
            case CODE:
                return ((Method) var.value).hashCode(); // Use Method's hashCode as the ID
            default:
                throw new IllegalStateException("Variable does not contain a type that can be converted to a double");
        }
    }

    // Helper method to convert Runtime to long
    private long toLong(Runtime var) {
        switch (var.type) {
            case INTEGER:
                return (long) var.value;
            case DOUBLE:
                return (long) ((double) var.value);
            case STRING:
                try {
                    return Long.parseLong((String) var.value);
                } catch (NumberFormatException e) {
                    return 0L; // Return zero if the string cannot be converted
                }
            case CODE:
                return ((Method) var.value).hashCode(); // Use Method's hashCode as the ID
            default:
                throw new IllegalStateException("Variable does not contain a type that can be converted to a long");
        }
    }

    // Other methods...
}

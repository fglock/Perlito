public class PerlVariable {

    // Enum to track the type of value stored in the PerlVariable
    public enum Type {
        UNDEFINED, INTEGER, DOUBLE, STRING, ARRAY_REF, HASH_REF, CODE_REF, OBJECT
    }

    private Type type;
    private Object value;

    // Constructor
    public PerlVariable() {
        this.type = Type.UNDEFINED;
        this.value = null;
    }

    // Set methods for different types
    public void setInteger(int value) {
        this.type = Type.INTEGER;
        this.value = value;
    }

    public void setDouble(double value) {
        this.type = Type.DOUBLE;
        this.value = value;
    }

    public void setString(String value) {
        this.type = Type.STRING;
        this.value = value;
    }

    public void setArrayRef(Object[] value) {
        this.type = Type.ARRAY_REF;
        this.value = value;
    }

    public void setHashRef(Map<String, Object> value) {
        this.type = Type.HASH_REF;
        this.value = value;
    }

    public void setCodeRef(Runnable value) {
        this.type = Type.CODE_REF;
        this.value = value;
    }

    public void setObject(Object value) {
        this.type = Type.OBJECT;
        this.value = value;
    }

    public void setUndefined() {
        this.type = Type.UNDEFINED;
        this.value = null;
    }

    // Get methods for different types with type checks
    public Integer getInteger() {
        if (this.type == Type.INTEGER) {
            return (Integer) this.value;
        }
        throw new IllegalStateException("Value is not an Integer");
    }

    public Double getDouble() {
        if (this.type == Type.DOUBLE) {
            return (Double) this.value;
        }
        throw new IllegalStateException("Value is not a Double");
    }

    public String getString() {
        if (this.type == Type.STRING) {
            return (String) this.value;
        }
        throw new IllegalStateException("Value is not a String");
    }

    public Object[] getArrayRef() {
        if (this.type == Type.ARRAY_REF) {
            return (Object[]) this.value;
        }
        throw new IllegalStateException("Value is not an Array Reference");
    }

    public Map<String, Object> getHashRef() {
        if (this.type == Type.HASH_REF) {
            return (Map<String, Object>) this.value;
        }
        throw new IllegalStateException("Value is not a Hash Reference");
    }

    public Runnable getCodeRef() {
        if (this.type == Type.CODE_REF) {
            return (Runnable) this.value;
        }
        throw new IllegalStateException("Value is not a Code Reference");
    }

    public Object getObject() {
        if (this.type == Type.OBJECT) {
            return this.value;
        }
        throw new IllegalStateException("Value is not an Object");
    }

    public boolean isUndefined() {
        return this.type == Type.UNDEFINED;
    }

    // Method to get the current type of the variable
    public Type getType() {
        return this.type;
    }

    // Optional: Override toString for debugging
    @Override
    public String toString() {
        return "PerlVariable{" +
                "type=" + type +
                ", value=" + value +
                '}';
    }
}

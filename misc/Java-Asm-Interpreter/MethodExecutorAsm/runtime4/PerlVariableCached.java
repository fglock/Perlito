import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;

public class PerlVariableCached {

    // Enum to track the type of value stored in the PerlVariableCached
    public enum Type {
        UNDEFINED, INTEGER, DOUBLE, STRING, ARRAY_REF, HASH_REF, CODE_REF, OBJECT
    }

    private Type type;
    private Object value;

    // Cached values for different contexts
    private transient Integer cachedInteger;
    private transient Double cachedDouble;
    private transient String cachedString;

    // Constructor
    public PerlVariableCached() {
        this.type = Type.UNDEFINED;
        this.value = null;
        clearCache();
    }

    // Set methods for different types
    public void setInteger(int value) {
        this.type = Type.INTEGER;
        this.value = value;
        clearCache();
    }

    public void setDouble(double value) {
        this.type = Type.DOUBLE;
        this.value = value;
        clearCache();
    }

    public void setString(String value) {
        this.type = Type.STRING;
        this.value = value;
        clearCache();
    }

    public void setArrayRef(Object[] value) {
        this.type = Type.ARRAY_REF;
        this.value = value;
        clearCache();
    }

    public void setHashRef(Map<String, Object> value) {
        this.type = Type.HASH_REF;
        this.value = value;
        clearCache();
    }

    public void setCodeRef(Runnable value) {
        this.type = Type.CODE_REF;
        this.value = value;
        clearCache();
    }

    public void setObject(Object value) {
        this.type = Type.OBJECT;
        this.value = value;
        clearCache();
    }

    public void setUndefined() {
        this.type = Type.UNDEFINED;
        this.value = null;
        clearCache();
    }

    // Get methods for different types with type checks and conversions
    public Integer getInteger() {
        if (this.cachedInteger != null) {
            return this.cachedInteger;
        }
        switch (this.type) {
            case INTEGER:
                return (Integer) this.value;
            case DOUBLE:
                this.cachedInteger = ((Double) this.value).intValue();
                return this.cachedInteger;
            case STRING:
                try {
                    this.cachedInteger = Integer.parseInt((String) this.value);
                } catch (NumberFormatException e) {
                    this.cachedInteger = 0;
                }
                return this.cachedInteger;
            default:
                throw new IllegalStateException("Value is not convertible to Integer");
        }
    }

    public Double getDouble() {
        if (this.cachedDouble != null) {
            return this.cachedDouble;
        }
        switch (this.type) {
            case DOUBLE:
                return (Double) this.value;
            case INTEGER:
                this.cachedDouble = ((Integer) this.value).doubleValue();
                return this.cachedDouble;
            case STRING:
                try {
                    this.cachedDouble = Double.parseDouble((String) this.value);
                } catch (NumberFormatException e) {
                    this.cachedDouble = 0.0;
                }
                return this.cachedDouble;
            default:
                throw new IllegalStateException("Value is not convertible to Double");
        }
    }

    public String getString() {
        if (this.cachedString != null) {
            return this.cachedString;
        }
        switch (this.type) {
            case STRING:
                return (String) this.value;
            case INTEGER:
                this.cachedString = this.value.toString();
                return this.cachedString;
            case DOUBLE:
                this.cachedString = this.value.toString();
                return this.cachedString;
            default:
                throw new IllegalStateException("Value is not convertible to String");
        }
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

    // Method to clear cached values
    private void clearCache() {
        this.cachedInteger = null;
        this.cachedDouble = null;
        this.cachedString = null;
    }

    // Optional: Override toString for debugging
    @Override
    public String toString() {
        return "PerlVariableCached{" +
                "type=" + type +
                ", value=" + value +
                '}';
    }
}

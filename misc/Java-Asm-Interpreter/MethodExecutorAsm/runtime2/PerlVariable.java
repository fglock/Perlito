abstract class PerlVariable {
    abstract PerlVariable add(PerlVariable other);
    abstract PerlVariable subtract(PerlVariable other);
    abstract PerlVariable multiply(PerlVariable other);
    abstract PerlVariable divide(PerlVariable other);
}

class PerlInt extends PerlVariable {
    private int value;

    public PerlInt(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public PerlVariable add(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlInt(this.value + ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value + ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for addition");
    }

    @Override
    public PerlVariable subtract(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlInt(this.value - ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value - ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for subtraction");
    }

    @Override
    public PerlVariable multiply(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlInt(this.value * ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value * ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for multiplication");
    }

    @Override
    public PerlVariable divide(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlDouble((double) this.value / ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value / ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for division");
    }
}

class PerlDouble extends PerlVariable {
    private double value;

    public PerlDouble(double value) {
        this.value = value;
    }

    public double getValue() {
        return value;
    }

    @Override
    public PerlVariable add(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value + ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value + ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for addition");
    }

    @Override
    public PerlVariable subtract(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value - ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value - ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for subtraction");
    }

    @Override
    public PerlVariable multiply(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value * ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value * ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for multiplication");
    }

    @Override
    public PerlVariable divide(PerlVariable other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value / ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value / ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for division");
    }
}

class PerlString extends PerlVariable {
    private String value;

    public PerlString(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    @Override
    public PerlVariable add(PerlVariable other) {
        return new PerlString(this.value + other.toString());
    }

    @Override
    public PerlVariable subtract(PerlVariable other) {
        throw new UnsupportedOperationException("Subtraction not supported on strings");
    }

    @Override
    public PerlVariable multiply(PerlVariable other) {
        throw new UnsupportedOperationException("Multiplication not supported on strings");
    }

    @Override
    public PerlVariable divide(PerlVariable other) {
        throw new UnsupportedOperationException("Division not supported on strings");
    }

    @Override
    public String toString() {
        return value;
    }
}

// Main class to demonstrate usage
public class Main {
    public static void main(String[] args) {
        PerlVariable var1 = new PerlInt(10);
        PerlVariable var2 = new PerlDouble(20.5);
        PerlVariable var3 = new PerlString("Hello");

        PerlVariable result = var1.add(var2); // Should return a PerlDouble with value 30.5
        System.out.println("10 + 20.5 = " + ((PerlDouble) result).getValue());

        result = var1.multiply(var2); // Should return a PerlDouble with value 205.0
        System.out.println("10 * 20.5 = " + ((PerlDouble) result).getValue());

        result = var3.add(new PerlString(" World")); // Should return a PerlString with value "Hello World"
        System.out.println("\"Hello\" + \" World\" = " + result.toString());
    }
}

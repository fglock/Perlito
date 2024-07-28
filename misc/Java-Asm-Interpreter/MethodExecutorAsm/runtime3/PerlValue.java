abstract class PerlValue {
    abstract PerlValue add(PerlValue other);
    abstract PerlValue subtract(PerlValue other);
    abstract PerlValue multiply(PerlValue other);
    abstract PerlValue divide(PerlValue other);
    abstract Object getValue();
}

class PerlInt extends PerlValue {
    private int value;

    public PerlInt(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public PerlValue add(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlInt(this.value + ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value + ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for addition");
    }

    @Override
    public PerlValue subtract(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlInt(this.value - ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value - ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for subtraction");
    }

    @Override
    public PerlValue multiply(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlInt(this.value * ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value * ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for multiplication");
    }

    @Override
    public PerlValue divide(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlDouble((double) this.value / ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value / ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for division");
    }
}

class PerlDouble extends PerlValue {
    private double value;

    public PerlDouble(double value) {
        this.value = value;
    }

    public double getValue() {
        return value;
    }

    @Override
    public PerlValue add(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value + ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value + ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for addition");
    }

    @Override
    public PerlValue subtract(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value - ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value - ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for subtraction");
    }

    @Override
    public PerlValue multiply(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value * ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value * ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for multiplication");
    }

    @Override
    public PerlValue divide(PerlValue other) {
        if (other instanceof PerlInt) {
            return new PerlDouble(this.value / ((PerlInt) other).getValue());
        } else if (other instanceof PerlDouble) {
            return new PerlDouble(this.value / ((PerlDouble) other).getValue());
        }
        throw new IllegalArgumentException("Unsupported type for division");
    }
}

class PerlString extends PerlValue {
    private String value;

    public PerlString(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    @Override
    public PerlValue add(PerlValue other) {
        return new PerlString(this.value + other.toString());
    }

    @Override
    public PerlValue subtract(PerlValue other) {
        throw new UnsupportedOperationException("Subtraction not supported on strings");
    }

    @Override
    public PerlValue multiply(PerlValue other) {
        throw new UnsupportedOperationException("Multiplication not supported on strings");
    }

    @Override
    public PerlValue divide(PerlValue other) {
        throw new UnsupportedOperationException("Division not supported on strings");
    }

    @Override
    public String toString() {
        return value;
    }
}

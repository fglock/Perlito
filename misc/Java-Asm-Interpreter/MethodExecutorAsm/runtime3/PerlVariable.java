class PerlVariable {
    private PerlValue value;

    public PerlVariable(PerlValue value) {
        this.value = value;
    }

    public PerlValue getValue() {
        return value;
    }

    public void setValue(PerlValue value) {
        this.value = value;
    }

    public PerlVariable add(PerlVariable other) {
        return new PerlVariable(this.value.add(other.getValue()));
    }

    public PerlVariable subtract(PerlVariable other) {
        return new PerlVariable(this.value.subtract(other.getValue()));
    }

    public PerlVariable multiply(PerlVariable other) {
        return new PerlVariable(this.value.multiply(other.getValue()));
    }

    public PerlVariable divide(PerlVariable other) {
        return new PerlVariable(this.value.divide(other.getValue()));
    }
}

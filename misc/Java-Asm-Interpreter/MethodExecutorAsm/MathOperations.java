public class MathOperations {
    private final long i; 
    public MathOperations(long i) {
        this.i = i;
    }
    public MathOperations(int i) {
        this.i = (long)i;
    }

    public String toString() {
        return String.valueOf(this.i);
    }

    public static MathOperations make(int a) {
        return new MathOperations(a);
    }

    public MathOperations add(int a, int b) {
        return new MathOperations(a + b);
    }

    public MathOperations add(int b) {
        return new MathOperations(this.i + b);
    }

    public MathOperations add(MathOperations b) {
        return new MathOperations(this.i + b.i);
    }

    public MathOperations multiply(int b) {
        return new MathOperations(this.i * b);
    }

    public MathOperations multiply(MathOperations b) {
        return new MathOperations(this.i * b.i);
    }
}

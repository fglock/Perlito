public class MathOperations {
    private int baseValue;

    public MathOperations(int baseValue) {
        this.baseValue = baseValue;
    }

    public int add(int a, int b) {
        return a + b;
    }

    public int multiply(int a, int b) {
        return a * b;
    }

    public int addToBase(int a) {
        return baseValue + a;
    }

    public int multiplyWithBase(int a) {
        return baseValue * a;
    }

    public static int staticMethod(int a, int b) {
        return a + b;
    }
}


public class MathOperations {
    private int baseValue;

    // Constructor
    public MathOperations(int baseValue) {
        this.baseValue = baseValue;
    }

    // Instance method
    public int add(int a, int b) {
        return a + b;
    }

    // Instance method
    public int multiply(int a, int b) {
        return a * b;
    }

    // Static method
    public static int staticMethod(int a, int b) {
        return a + b;
    }

    // Instance method using baseValue
    public int addToBase(int a) {
        return this.baseValue + a;
    }

    // Instance method using baseValue
    public int multiplyWithBase(int a) {
        return this.baseValue * a;
    }

    public static void main(String[] args) {
        // Example usage
        MathOperations mathOps = new MathOperations(10);
        System.out.println(mathOps.add(5, 3)); // Output: 8
        System.out.println(mathOps.multiply(2, 4)); // Output: 8
        System.out.println(MathOperations.staticMethod(5, 3)); // Output: 8
        System.out.println(mathOps.addToBase(5)); // Output: 15
        System.out.println(mathOps.multiplyWithBase(2)); // Output: 20
    }
}


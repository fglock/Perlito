public class MathOperations {
    private int baseValue;

    public MathOperations(int baseValue) {
        this.baseValue = baseValue;
    }

    public static int staticMethod(int a, int b) {
        return a + b;
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

    public static void noArgStaticMethod() {
        System.out.println("No-arg static method called");
    }

    public void noArgInstanceMethod() {
        System.out.println("No-arg instance method called");
    }
}


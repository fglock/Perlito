import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class UnaryOperatorBenchmark {

    private static final Set<String> UNARY_OP_SET = new HashSet<>(
        Arrays.asList(
            "!", "\\", "-", "+", "--", "++", // operators
            "$", "@", "%", "*", "&", "$#"    // sigils
        )
    );

    public static boolean isUnaryOperatorSet(String op) {
        return UNARY_OP_SET.contains(op);
    }

    public static boolean isUnaryOperatorSwitch(String op) {
        switch (op) {
            case "!":
            case "\\":
            case "-":
            case "+":
            case "--":
            case "++":
            case "$":
            case "@":
            case "%":
            case "*":
            case "&":
            case "$#":
                return true;
            default:
                return false;
        }
    }

    public static void main(String[] args) {
        String[] testOperators = {"!", "\\", "-", "+", "--", "++", "$", "@", "%", "*", "&", "$#", "foo", "bar", "baz"};
        int iterations = 10_000_000;

        // Warm-up phase
        for (int i = 0; i < iterations; i++) {
            isUnaryOperatorSet(testOperators[i % testOperators.length]);
            isUnaryOperatorSwitch(testOperators[i % testOperators.length]);
        }

        // Benchmark HashSet approach
        long startTime = System.nanoTime();
        for (int i = 0; i < iterations; i++) {
            isUnaryOperatorSet(testOperators[i % testOperators.length]);
        }
        long endTime = System.nanoTime();
        long durationSet = endTime - startTime;

        // Benchmark switch statement approach
        startTime = System.nanoTime();
        for (int i = 0; i < iterations; i++) {
            isUnaryOperatorSwitch(testOperators[i % testOperators.length]);
        }
        endTime = System.nanoTime();
        long durationSwitch = endTime - startTime;

        // Print results
        System.out.println("HashSet approach duration: " + durationSet / 1_000_000.0 + " ms");
        System.out.println("Switch statement approach duration: " + durationSwitch / 1_000_000.0 + " ms");
    }
}

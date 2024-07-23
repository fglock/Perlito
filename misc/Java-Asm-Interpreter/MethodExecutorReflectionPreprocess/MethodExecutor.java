import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Arrays;

public class MethodExecutor {
    public static Object execute(Object callable, Object targetOrArgs, Object... args) throws Exception {
        if (callable instanceof Method) {
            Method method = (Method) callable;
            System.out.println("Invoking method: " + method.getName() + " on target: " + targetOrArgs + " with arguments: " + Arrays.toString(args));
            return method.invoke(targetOrArgs, args);
        } else if (callable instanceof Constructor) {
            Constructor<?> constructor = (Constructor<?>) callable;
            System.out.println("Invoking constructor: " + constructor.getName() + " with arguments: " + Arrays.toString((Object[]) targetOrArgs));
            return constructor.newInstance((Object[]) targetOrArgs);
        }
        throw new IllegalArgumentException("Unsupported callable type");
    }

    public static void main(String[] args) {
        try {
            Object[][] methodCalls = {
                {MathOperations.class, "new", 10}, // Constructor call with argument
                {MathOperations.class, "staticMethod", 5, 3}, // Static method call
                {new MathOperations(10), "add", 5, 3}, // Instance method call
                {new MathOperations(10), "multiply", 2, 4}, // Instance method call
                {new MathOperations(10), "addToBase", 5}, // Instance method call using baseValue
                {new MathOperations(10), "multiplyWithBase", 2}, // Instance method call using baseValue
                // Nested method calls
                {MathOperations.class, "new", new Object[]{MathOperations.class, "staticMethod", 5, 3}}, // Constructor with static method result as argument
                {new MathOperations(10), "add", new Object[]{MathOperations.class, "staticMethod", 5, 3}, 3}, // Instance method with static method result as argument
                {new MathOperations(10), "multiply", new Object[]{new MathOperations(5), "add", 2, 3}, 4}, // Instance method with another instance method result as argument
                // More levels of nesting
                {MathOperations.class, "new", new Object[]{MathOperations.class, "staticMethod", new Object[]{MathOperations.class, "staticMethod", 2, 3}, 3}}
            };

            Object[][] preprocessedCalls = MethodPreprocessor.preprocess(methodCalls);

            for (Object[] call : preprocessedCalls) {
                Object callable = call[0];
                Object targetOrArgs = call[1];
                Object[] callArgs = (Object[]) call[2]; // Renamed to callArgs

                // Evaluate nested calls
                for (int i = 0; i < callArgs.length; i++) {
                    if (callArgs[i] instanceof Object[]) {
                        callArgs[i] = executeNestedCall((Object[]) callArgs[i]);
                    }
                }

                Object result = execute(callable, targetOrArgs, callArgs);
                System.out.println("Result: " + result);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Object executeNestedCall(Object[] nestedCall) throws Exception {
        Object callable = nestedCall[0];
        Object targetOrArgs = nestedCall[1];
        Object[] args = (Object[]) nestedCall[2];

        // Evaluate further nested calls
        for (int i = 0; i < args.length; i++) {
            if (args[i] instanceof Object[]) {
                args[i] = executeNestedCall((Object[]) args[i]);
            }
        }

        return execute(callable, targetOrArgs, args);
    }
}


import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Arrays;

public class MethodExecutor {
    public static Object execute(Object callable, Object target, Object... args) throws Exception {
        if (callable instanceof Method) {
            Method method = (Method) callable;
            System.out.println("Invoking method: " + method.getName() + " on target: " + target + " with arguments: " + Arrays.toString(args));
            return method.invoke(target, args);
        } else if (callable instanceof Constructor) {
            Constructor<?> constructor = (Constructor<?>) callable;
            System.out.println("Invoking constructor: " + constructor.getName() + " with arguments: " + Arrays.toString(args));
            return constructor.newInstance(args);
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
                {MathOperations.class, "noArgStaticMethod"}, // Static method call with no arguments
                {new MathOperations(10), "noArgInstanceMethod"}, // Instance method call with no arguments
                // Nested method calls
                {MathOperations.class, "new", new Object[]{MathOperations.class, "staticMethod", 5, 3}}, // Constructor with static method result as argument
                {new MathOperations(10), "add", new Object[]{MathOperations.class, "staticMethod", 5, 3}, 3}, // Instance method with static method result as argument
                {new MathOperations(10), "multiply", new Object[]{new MathOperations(5), "add", 2, 3}, 4}, // Instance method with another instance method result as argument
                // More levels of nesting
                {MathOperations.class, "new", new Object[]{MathOperations.class, "staticMethod", new Object[]{MathOperations.class, "staticMethod", 2, 3}, 3}}
            };

            Object[][] preprocessedCalls = MethodPreprocessor.preprocess(methodCalls);

            for (Object[] call : preprocessedCalls) {
                if (call.length < 3) {
                    throw new IllegalArgumentException("Preprocessed call array length is less than 3: " + Arrays.toString(call));
                }

                Object callable = call[0];
                Object target = call[1];
                Object[] callArgs = (Object[]) call[2];

                // Evaluate nested calls
                for (int i = 0; i < callArgs.length; i++) {
                    if (callArgs[i] instanceof Object[]) {
                        callArgs[i] = executeNestedCall((Object[]) callArgs[i]);
                    }
                }

                Object result = execute(callable, target, callArgs);
                System.out.println("Result: " + result);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Object executeNestedCall(Object[] nestedCall) throws Exception {
        if (nestedCall.length < 3) {
            throw new IllegalArgumentException("Nested call array length is less than 3: " + Arrays.toString(nestedCall));
        }

        Object callable = nestedCall[0];
        Object target = nestedCall[1];
        Object[] args = (Object[]) nestedCall[2];

        // Evaluate further nested calls
        for (int i = 0; i < args.length; i++) {
            if (args[i] instanceof Object[]) {
                args[i] = executeNestedCall((Object[]) args[i]);
            }
        }

        return execute(callable, target, args);
    }
}


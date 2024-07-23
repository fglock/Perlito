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
                {new MathOperations(10), "multiplyWithBase", 2} // Instance method call using baseValue
            };

            // Preprocess to resolve methods and constructors
            Object[][] preprocessedCalls = MethodPreprocessor.preprocess(methodCalls);

            // Execute preprocessed methods and constructors
            for (Object[] call : preprocessedCalls) {
                Object callable = call[0];
                Object targetOrArgs = call[1];
                Object[] methodArgs = (Object[]) call[2];
                Object result = execute(callable, targetOrArgs, methodArgs);
                System.out.println(result);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

import java.lang.reflect.Method;

public class MethodExecutor {
    public static Object execute(Method method, Object target, Object... args) throws Exception {
        return method.invoke(target, args);
    }

    public static void main(String[] args) {
        try {
            MathOperations mathOps = new MathOperations();
            Object[][] methodCalls = {
                {mathOps, "add", 5, 3},
                {mathOps, "multiply", 2, 4}
            };

            // Preprocess to resolve methods
            Object[][] preprocessedCalls = MethodPreprocessor.preprocess(methodCalls);

            // Execute preprocessed methods
            for (Object[] call : preprocessedCalls) {
                Method method = (Method) call[0];
                Object target = call[1];
                Object[] methodArgs = (Object[]) call[2];
                Object result = execute(method, target, methodArgs);
                System.out.println(result);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

import java.lang.reflect.Method;

public class MethodPreprocessor {

    public static Object[][] preprocess(Object[][] methodCalls) throws Exception {
        Object[][] preprocessedCalls = new Object[methodCalls.length][];
        for (int i = 0; i < methodCalls.length; i++) {
            Object[] call = methodCalls[i];
            Object target = call[0];
            String methodName = (String) call[1];
            Object[] args = new Object[call.length - 2];
            System.arraycopy(call, 2, args, 0, call.length - 2);

            Class<?>[] argTypes = new Class[args.length];
            for (int j = 0; j < args.length; j++) {
                argTypes[j] = args[j].getClass();
            }

            Method method = target.getClass().getMethod(methodName, argTypes);
            preprocessedCalls[i] = new Object[]{method, target, args};
        }
        return preprocessedCalls;
    }
}

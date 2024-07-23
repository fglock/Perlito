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
                argTypes[j] = (args[j] == null) ? Object.class : getPrimitiveClass(args[j].getClass());
            }

            Method method = target.getClass().getMethod(methodName, argTypes);
            preprocessedCalls[i] = new Object[]{method, target, args};
        }
        return preprocessedCalls;
    }

    private static Class<?> getPrimitiveClass(Class<?> clazz) {
        if (clazz == Integer.class) {
            return int.class;
        } else if (clazz == Double.class) {
            return double.class;
        } else if (clazz == Float.class) {
            return float.class;
        } else if (clazz == Long.class) {
            return long.class;
        } else if (clazz == Boolean.class) {
            return boolean.class;
        } else if (clazz == Byte.class) {
            return byte.class;
        } else if (clazz == Short.class) {
            return short.class;
        } else if (clazz == Character.class) {
            return char.class;
        }
        return clazz;
    }
}

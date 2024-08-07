import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;

public class MethodPreprocessor {

    public static Object[][] preprocess(Object[][] methodCalls) throws Exception {
        Object[][] preprocessedCalls = new Object[methodCalls.length][];
        for (int i = 0; i < methodCalls.length; i++) {
            preprocessedCalls[i] = preprocessCall(methodCalls[i]);
            System.out.println("Preprocessed call: " + Arrays.toString(preprocessedCalls[i]));
        }
        return preprocessedCalls;
    }

    private static Object[] preprocessCall(Object[] call) throws Exception {
        if (call.length < 2) {
            throw new IllegalArgumentException("Call array length is less than 2: " + Arrays.toString(call));
        }

        if (call[1] instanceof String && call[1].equals("new")) {
            // Handle constructor
            Class<?> targetClass = (Class<?>) call[0];
            Object[] args = new Object[call.length - 2];
            System.arraycopy(call, 2, args, 0, call.length - 2);

            Class<?>[] argTypes = new Class[args.length];
            for (int j = 0; j < args.length; j++) {
                if (args[j] instanceof Object[]) {
                    args[j] = preprocessCall((Object[]) args[j]);
                    argTypes[j] = getReturnType((Object[]) args[j]);
                } else {
                    argTypes[j] = getPrimitiveType(args[j].getClass());
                }
            }

            System.out.println("Attempting to find constructor for class: " + targetClass.getName() + " with arguments: " + Arrays.toString(argTypes));
            Constructor<?> constructor = targetClass.getConstructor(argTypes);
            return new Object[]{constructor, null, args};
        } else {
            // Handle method (instance or static)
            Object target = call[0];
            String methodName = (String) call[1];
            Object[] args = new Object[call.length - 2];
            System.arraycopy(call, 2, args, 0, call.length - 2);

            Class<?>[] argTypes = new Class[args.length];
            for (int j = 0; j < args.length; j++) {
                if (args[j] instanceof Object[]) {
                    args[j] = preprocessCall((Object[]) args[j]);
                    argTypes[j] = getReturnType((Object[]) args[j]);
                } else {
                    argTypes[j] = getPrimitiveType(args[j].getClass());
                }
            }

            System.out.println("Attempting to find method: " + methodName + " for class: " + target.getClass().getName() + " with arguments: " + Arrays.toString(argTypes));
            Method method;
            if (target instanceof Class) {
                method = ((Class<?>) target).getMethod(methodName, argTypes);
                return new Object[]{method, null, args};
            } else {
                method = target.getClass().getMethod(methodName, argTypes);
                if (Modifier.isStatic(method.getModifiers())) {
                    return new Object[]{method, null, args};
                } else {
                    return new Object[]{method, target, args};
                }
            }
        }
    }

    private static Class<?> getReturnType(Object[] preprocessedCall) {
        if (preprocessedCall[0] instanceof Method) {
            return ((Method) preprocessedCall[0]).getReturnType();
        } else if (preprocessedCall[0] instanceof Constructor) {
            return ((Constructor<?>) preprocessedCall[0]).getDeclaringClass();
        }
        throw new IllegalArgumentException("Unsupported callable type");
    }

    private static Class<?> getPrimitiveType(Class<?> clazz) {
        if (clazz == Integer.class) return int.class;
        if (clazz == Double.class) return double.class;
        if (clazz == Float.class) return float.class;
        if (clazz == Long.class) return long.class;
        if (clazz == Short.class) return short.class;
        if (clazz == Byte.class) return byte.class;
        if (clazz == Boolean.class) return boolean.class;
        if (clazz == Character.class) return char.class;
        return clazz;
    }
}


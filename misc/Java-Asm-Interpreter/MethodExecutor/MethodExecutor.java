import java.io.PrintStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class MethodExecutor {

    public static MethodResult execute(Object[] data) throws Exception {
        if (data == null || data.length < 2) {
            throw new IllegalArgumentException("Invalid data structure");
        }

        Object target = data[0];
        String methodName = (String) data[1];
        Object[] args = new Object[data.length - 2];
        int argIndex = 0;

        for (int i = 2; i < data.length; i++) {
            if (data[i] instanceof Object[]) {
                MethodResult result = execute((Object[]) data[i]);
                if (!result.discard) {
                    args[argIndex++] = result.result;
                }
            } else {
                args[argIndex++] = data[i];
            }
        }

        // Trim the args array to the actual number of arguments
        Object[] trimmedArgs = new Object[argIndex];
        System.arraycopy(args, 0, trimmedArgs, 0, argIndex);

        // System.out.println("Invoking method: " + methodName + " on class: " + target.getClass().getName());
        // System.out.println("Arguments: ");
        // for (Object arg : trimmedArgs) {
        //     System.out.println("  - " + (arg == null ? "null" : arg.getClass().getName()));
        // }

        Class<?>[] argTypes = new Class[trimmedArgs.length];
        for (int i = 0; i < trimmedArgs.length; i++) {
            argTypes[i] = (trimmedArgs[i] == null) ? Object.class : getPrimitiveClass(trimmedArgs[i].getClass());
        }

        Method method = target.getClass().getMethod(methodName, argTypes);
        // System.out.println("Resolved method: " + method.getName() + " with parameter types: ");
        // for (Class<?> paramType : method.getParameterTypes()) {
        //     System.out.println("  - " + paramType.getName());
        // }

        Object result = method.invoke(target, trimmedArgs);

        // If the method returns void, set discard flag to true
        boolean discard = method.getReturnType() == void.class;

        return new MethodResult(result, discard);
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

    public static void main(String[] args) {
        try {
            // Example usage
            MathOperations mathOps = new MathOperations();
            Object[] data = {
                System.out, "println", "Starting execution...",
                new Object[]{System.out, "println", new Object[]{mathOps, "add", 5, 3}},
                new Object[]{System.out, "println", new Object[]{mathOps, "multiply", 2, 4}},
                new Object[]{System.out, "println", "Execution finished."}
            };
            execute(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

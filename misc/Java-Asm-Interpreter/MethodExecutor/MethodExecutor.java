import java.io.PrintStream;
import java.lang.reflect.Method;

public class MethodExecutor {

    public static Object execute(Object[] data) throws Exception {
        if (data == null || data.length < 2) {
            throw new IllegalArgumentException("Invalid data structure");
        }

        Object target = data[0];
        String methodName = (String) data[1];
        Object[] args = new Object[data.length - 2];

        for (int i = 2; i < data.length; i++) {
            if (data[i] instanceof Object[]) {
                args[i - 2] = execute((Object[]) data[i]);
            } else {
                args[i - 2] = data[i];
            }
        }

        return invokeMethod(target, methodName, args);
    }

    private static Object invokeMethod(Object target, String methodName, Object[] args) throws Exception {
        if (target instanceof PrintStream && methodName.equals("println")) {
            return invokePrintStreamMethod((PrintStream) target, args);
        }

        Class<?>[] argTypes = new Class[args.length];
        for (int i = 0; i < args.length; i++) {
            argTypes[i] = (args[i] == null) ? Object.class : getPrimitiveClass(args[i].getClass());
        }

        Method method = target.getClass().getMethod(methodName, argTypes);
        return method.invoke(target, args);
    }

    private static Object invokePrintStreamMethod(PrintStream target, Object[] args) {
        if (args.length == 0) {
            target.println();
        } else if (args.length == 1) {
            if (args[0] == null) {
                target.println((String) null);
            } else if (args[0] instanceof String) {
                target.println((String) args[0]);
            } else if (args[0] instanceof Integer) {
                target.println((Integer) args[0]);
            } else if (args[0] instanceof Double) {
                target.println((Double) args[0]);
            } else if (args[0] instanceof Float) {
                target.println((Float) args[0]);
            } else if (args[0] instanceof Long) {
                target.println((Long) args[0]);
            } else if (args[0] instanceof Boolean) {
                target.println((Boolean) args[0]);
            } else if (args[0] instanceof Character) {
                target.println((Character) args[0]);
            } else {
                target.println(args[0].toString());
            }
        } else {
            StringBuilder sb = new StringBuilder();
            for (Object arg : args) {
                if (arg == null) {
                    sb.append("null");
                } else {
                    sb.append(arg.toString());
                }
                sb.append(" ");
            }
            target.println(sb.toString().trim());
        }
        return null;
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

import java.lang.reflect.Method;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class MethodExecutor {
    private static final ConcurrentMap<MethodKey, Method> methodCache = new ConcurrentHashMap<>();
    private static Method lastMethod = null;
    private static MethodKey lastMethodKey = null;

    public static Object execute(Object target, String methodName, Object... args) throws Exception {
        MethodKey methodKey = new MethodKey(target.getClass(), methodName);

        if (methodKey.equals(lastMethodKey)) {
            try {
                return lastMethod.invoke(target, args);
            } catch (IllegalArgumentException e) {
                // Method invocation failed, fall back to full resolution
            }
        }

        Class<?>[] argTypes = new Class[args.length];
        for (int i = 0; i < args.length; i++) {
            argTypes[i] = args[i].getClass();
        }

        methodKey = new MethodKey(target.getClass(), methodName, argTypes);
        Method method = methodCache.computeIfAbsent(methodKey, key -> {
            try {
                return target.getClass().getMethod(methodName, argTypes);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
        });

        lastMethod = method;
        lastMethodKey = methodKey;

        return method.invoke(target, args);
    }

    private static class MethodKey {
        private final Class<?> targetClass;
        private final String methodName;
        private final Class<?>[] argTypes;
        private final int hashCode;

        MethodKey(Class<?> targetClass, String methodName) {
            this(targetClass, methodName, null);
        }

        MethodKey(Class<?> targetClass, String methodName, Class<?>[] argTypes) {
            this.targetClass = targetClass;
            this.methodName = methodName;
            this.argTypes = argTypes;
            this.hashCode = computeHashCode();
        }

        private int computeHashCode() {
            int result = targetClass.hashCode();
            result = 31 * result + methodName.hashCode();
            if (argTypes != null) {
                result = 31 * result + java.util.Arrays.hashCode(argTypes);
            }
            return result;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            MethodKey methodKey = (MethodKey) o;
            return targetClass.equals(methodKey.targetClass) &&
                   methodName.equals(methodKey.methodName) &&
                   java.util.Arrays.equals(argTypes, methodKey.argTypes);
        }

        @Override
        public int hashCode() {
            return hashCode;
        }
    }

    public static void main(String[] args) {
        try {
            MathOperations mathOps = new MathOperations();
            System.out.println(execute(mathOps, "add", 5, 3)); // Output: 8
            System.out.println(execute(mathOps, "multiply", 2, 4)); // Output: 8
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

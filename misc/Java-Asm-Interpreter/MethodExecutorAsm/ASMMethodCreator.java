import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.lang.reflect.Method;

public class ASMMethodCreator implements Opcodes {

    public static byte[] createClassWithMethod(Object[] data) throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_8, ACC_PUBLIC, "GeneratedClass", null, "java/lang/Object", null);

        // Create default constructor
        MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();

        // Create the method
        mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "generatedMethod", "()Ljava/lang/Object;", null, null);
        mv.visitCode();

        // Process the input data
        processInstructions(mv, data);

        // Return the last value
        mv.visitInsn(ARETURN);

        // Max stack and local variables
        mv.visitMaxs(0, 0);
        mv.visitEnd();

        cw.visitEnd();

        return cw.toByteArray();
    }

    private static void processInstructions(MethodVisitor mv, Object[] data) throws Exception {
        if (data.length < 2) {
            throw new IllegalArgumentException("Invalid data structure");
        }

        Object target = data[0];
        String methodName = (String) data[1];
        Object[] args = new Object[data.length - 2];
        System.arraycopy(data, 2, args, 0, args.length);

        // Load the target object
        mv.visitLdcInsn(target);

        // Load the arguments
        for (Object arg : args) {
            if (arg instanceof Object[]) {
                processInstructions(mv, (Object[]) arg);
            } else if (arg instanceof Integer) {
                mv.visitLdcInsn(arg);
            } else if (arg instanceof String) {
                mv.visitLdcInsn(arg);
            }
        }

        // Invoke the method
        String descriptor = getMethodDescriptor(args);
        mv.visitMethodInsn(INVOKEVIRTUAL, target.getClass().getName().replace('.', '/'), methodName, descriptor, false);
    }

    private static String getMethodDescriptor(Object[] args) {
        StringBuilder descriptor = new StringBuilder("(");
        for (Object arg : args) {
            if (arg instanceof Integer) {
                descriptor.append("I");
            } else if (arg instanceof String) {
                descriptor.append("Ljava/lang/String;");
            }
        }
        descriptor.append(")Ljava/lang/Object;");
        return descriptor.toString();
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

            // Create the class
            byte[] classData = createClassWithMethod(data);
            CustomClassLoader loader = new CustomClassLoader();
            Class<?> generatedClass = loader.defineClass("GeneratedClass", classData);

            // Print debug statement
            System.out.println("Generated class: " + generatedClass.getName());

            // Call the generated method using reflection
            Method method = generatedClass.getMethod("generatedMethod");
            Object result = method.invoke(null);

            // Print the result
            System.out.println("Result of generatedMethod: " + result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}


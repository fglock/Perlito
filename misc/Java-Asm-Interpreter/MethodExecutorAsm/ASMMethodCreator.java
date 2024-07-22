import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.lang.reflect.Method;

public class ASMMethodCreator implements Opcodes {

    public static byte[] createClassWithMethod(Object[] data) throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_8, ACC_PUBLIC, "GeneratedClass", null, "java/lang/Object", null);

        // Create default constructor
        System.out.println("Create default constructor");
        MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();

        // Create the method
        System.out.println("Create the method");

        // String return_type = "()Ljava/lang/Object;";    // returns an Object
        String return_type = "()V";                     // returns void

        mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "generatedMethod", return_type, null, null);
        mv.visitCode();

        // Process the input data
        System.out.println("Process the input data");
        processInstructions(mv, data);

        // Return the last value
        System.out.println("Return the last value");
        mv.visitInsn(Opcodes.RETURN);   // returns void
        // mv.visitInsn(Opcodes.ARETURN);      // returns an Object

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
    System.out.println("Load the target object " + target);
    if (target instanceof Class<?>) {
        // If the target is a class, it means we're calling a static method
        mv.visitLdcInsn(org.objectweb.asm.Type.getType((Class<?>) target));
    } else if (target instanceof String) {
        mv.visitLdcInsn(target);
    } else if (target instanceof Integer) {
        mv.visitLdcInsn(target);
    } else {
        // Assuming the target is an instance object reference
        // You'll need to load the correct reference to this object in the local variable
        mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
    }

    // Load the arguments
    for (Object arg : args) {
        System.out.println("Load argument: " + arg);
        if (arg instanceof Object[]) {
            processInstructions(mv, (Object[]) arg);
        } else if (arg instanceof Integer) {
            mv.visitLdcInsn(arg);
        } else if (arg instanceof String) {
            mv.visitLdcInsn(arg);
        } else if (arg instanceof Class<?>) {
            mv.visitLdcInsn(org.objectweb.asm.Type.getType((Class<?>) arg));
        } else {
            throw new IllegalArgumentException("Unsupported argument type: " + arg.getClass());
        }
    }

    // Invoke the method
    String descriptor = getMethodDescriptor(target, methodName, args);
    if (target instanceof Class<?>) {
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, ((Class<?>) target).getName().replace('.', '/'), methodName, descriptor, false);
    } else {
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, target.getClass().getName().replace('.', '/'), methodName, descriptor, false);
    }
}

private static String getMethodDescriptor(Object target, String methodName, Object[] args) throws NoSuchMethodException {
    Class<?>[] argTypes = new Class[args.length];
    for (int i = 0; i < args.length; i++) {
        if (args[i] instanceof Integer) {
            argTypes[i] = int.class;
        } else if (args[i] instanceof String) {
            argTypes[i] = String.class;
        } else if (args[i] instanceof Class<?>) {
            argTypes[i] = (Class<?>) args[i];
        } else {
            throw new IllegalArgumentException("Unsupported argument type: " + args[i].getClass());
        }
    }

    Method method;
    if (target instanceof Class<?>) {
        method = ((Class<?>) target).getMethod(methodName, argTypes);
    } else {
        method = target.getClass().getMethod(methodName, argTypes);
    }

    return org.objectweb.asm.Type.getMethodDescriptor(method);
}


    // private static void processInstructions(MethodVisitor mv, Object[] data) throws Exception {
    //     if (data.length < 2) {
    //         throw new IllegalArgumentException("Invalid data structure");
    //     }
    // 
    //     Object target = data[0];
    //     String methodName = (String) data[1];
    //     Object[] args = new Object[data.length - 2];
    //     System.arraycopy(data, 2, args, 0, args.length);
    // 
    //     // Load the target object
    //     System.out.println("Load the target object " + target);
    //     if (target instanceof Class<?>) {
    //         // If the target is a class, it means we're calling a static method
    //         mv.visitLdcInsn( org.objectweb.asm.Type.getType((Class<?>) target));
    //     } else {
    //         // Otherwise, it's an instance method, load the instance
    //         mv.visitVarInsn(Opcodes.ALOAD, 0); // Assuming 'this' is at index 0
    //     }
    // 
    //     // Load the arguments
    //     for (Object arg : args) {
    //         System.out.println("Load argument: " + arg);
    //         if (arg instanceof Object[]) {
    //             processInstructions(mv, (Object[]) arg);
    //         } else if (arg instanceof Integer) {
    //             mv.visitLdcInsn(arg);
    //         } else if (arg instanceof String) {
    //             mv.visitLdcInsn(arg);
    //         } else if (arg instanceof Class<?>) {
    //             mv.visitLdcInsn( org.objectweb.asm.Type.getType((Class<?>) arg));
    //         } else {
    //             throw new IllegalArgumentException("Unsupported argument type: " + arg.getClass());
    //         }
    //     }
    // 
    //     // Invoke the method
    //     String descriptor = getMethodDescriptor(target, methodName, args);
    //     if (target instanceof Class<?>) {
    //         mv.visitMethodInsn(Opcodes.INVOKESTATIC, ((Class<?>) target).getName().replace('.', '/'), methodName, descriptor, false);
    //     } else {
    //         mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, target.getClass().getName().replace('.', '/'), methodName, descriptor, false);
    //     }
    // }
    
    // private static String getMethodDescriptor(Object target, String methodName, Object[] args) throws NoSuchMethodException {
    //     Class<?>[] argTypes = new Class[args.length];
    //     for (int i = 0; i < args.length; i++) {
    //         // if (args[i] instanceof Integer) {
    //         //     argTypes[i] = int.class;
    //         // } else if (args[i] instanceof String) {
    //         //     argTypes[i] = String.class;
    //         // } else if (args[i] instanceof Class<?>) {
    //         //     argTypes[i] = (Class<?>) args[i];
    //         // } else {
    //         //     throw new IllegalArgumentException("Unsupported argument type: " + args[i].getClass());
    //         // }

    //         argTypes[i] = (args[i] == null) ? Object.class : getPrimitiveClass(args[i].getClass());
    //         System.out.println("type " + i + ": " + argTypes[i]);
    //     }
    //     System.out.println("call class " + target);
    //     System.out.println("call method " + methodName);
    // 
    //     Method method;
    //     if (target instanceof Class<?>) {
    //         method = ((Class<?>) target).getMethod(methodName, argTypes);
    //     } else {
    //         method = target.getClass().getMethod(methodName, argTypes);
    //     }
    // 
    //     return org.objectweb.asm.Type.getMethodDescriptor(method);
    // }

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
            // Object[] data = {
            //     System.out, "println", "Starting execution...",
            //     new Object[]{System.out, "println", new Object[]{mathOps, "add", 5, 3}},
            //     new Object[]{System.out, "println", new Object[]{mathOps, "multiply", 2, 4}},
            //     new Object[]{System.out, "println", "Execution finished."}
            // };

            // Object[] data = {
            //     System.out, "println", new Object[]{mathOps, "add", 5, 3
            // }};

            Object[] data = { System.out, "println", "123" };

            // Create the class
            System.out.println("createClassWithMethod");
            byte[] classData = createClassWithMethod(data);
            CustomClassLoader loader = new CustomClassLoader();
            Class<?> generatedClass = loader.defineClass("GeneratedClass", classData);

            // Print debug statement
            System.out.println("Generated class: " + generatedClass.getName());

            // Call the generated method using reflection
            System.out.println("generatedClass.getMethod");
            Method method = generatedClass.getMethod("generatedMethod");

            System.out.println("invoke");
            method.invoke(null);    // println returns void
            // Object result = method.invoke(null);

            // Print the result
            // System.out.println("Result of generatedMethod: " + result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}


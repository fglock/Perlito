import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.lang.reflect.Method;

public class ASMMethodCreator implements Opcodes {

    public static byte[] createClassWithMethod(Object[][] data) throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_8, ACC_PUBLIC, "GeneratedClass", null, "java/lang/Object", null);

        // Add "env" field to the class
        cw.visitField(Opcodes.ACC_PUBLIC, "env", "LRuntime;", null, null).visitEnd();

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

        String return_type = "(LRuntime;)Ljava/lang/Object;";    // takes an object, returns an Object
        // String return_type = "()Ljava/lang/Object;";    // returns an Object
        // String return_type = "()V";                     // returns void

        // method is public static
        mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "generatedMethod", return_type, null, null);
        mv.visitCode();

        // Process the input data
        for (int i = 0; i < data.length; i++) {
            System.out.println("Process the input data");
            processInstructions(mv, data[i]);   // XXX check returnClass of the last argument (void, int, Object)
        }

        // Return the last value
        System.out.println("Return the last value");
        // mv.visitInsn(Opcodes.RETURN);   // returns void
        mv.visitInsn(Opcodes.ARETURN);      // returns an Object

        // Max stack and local variables
        mv.visitMaxs(0, 0);
        mv.visitEnd();

        cw.visitEnd();

        return cw.toByteArray();
    }

    private static Class<?> processInstructions(MethodVisitor mv, Object[] data) throws Exception {
        if (data.length < 2) {
            throw new IllegalArgumentException("Invalid data structure");
        }
    
        Object target = data[0];
        Object[] args = new Object[data.length - 2];
        System.arraycopy(data, 2, args, 0, args.length);
    
        boolean targetIsInstance = true;
        boolean isReturn = false;
        Class<?> targetClass;

        // Load the target object
        System.out.println("Load the target object " + target);
        if (target instanceof Object[]) {
            //  { new Object[]{ Runtime.class, "make", 5 }, "add", 5 },
            targetClass = processInstructions(mv, (Object[]) target);
            System.out.println(" target is instance of: " + targetClass);
        } else if (target instanceof Class<?>) {
            targetIsInstance = false;       // If the target is a class, it means we're calling a static method
            targetClass = (Class<?>)target;
            System.out.println(" is Class");
            String methodName = (String) data[1];
            if ( methodName.equals("new") ) {
                // we are we calling a constructor
                System.out.println(" calling a constructor");
                // TODO
                //      mv.visitTypeInsn(Opcodes.NEW, "java/lang/Integer"); // Create a new Integer object
                //      mv.visitInsn(Opcodes.DUP); // Duplicate the top operand stack value
                //      mv.visitVarInsn(Opcodes.ILOAD, 1); // Load the method argument (int value)
                //      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V", false); // Call the Integer constructor

            }
            mv.visitLdcInsn(org.objectweb.asm.Type.getType((Class<?>) target));
        } else if (target instanceof String) {
            System.out.println(" is String");

            if ( target.equals("ARG")) {
                // { "ARG", 0, int.class }   { ARG, index, type }
                mv.visitVarInsn(ALOAD, (int)(data[1]));
                return (Class<?>)(data[2]);   // process returnClass
            }
            if ( target.equals("RETURN")) {
                // { "RETURN", null, new Object[]{ Runtime.class, "make", 5 } }
                System.out.println(" calling return");
                targetClass = Runtime.class;
                isReturn = true;
            }
            else {
                targetClass = target.getClass();
                mv.visitLdcInsn(target);
            }
        } else if (target instanceof Integer) {
            System.out.println(" is Integer");
            targetClass = target.getClass();
            mv.visitLdcInsn(target);
        } else if (target instanceof java.io.PrintStream) {
            System.out.println(" is " + target);
            targetClass = target.getClass();
            mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        } else {
            throw new IllegalArgumentException("Unsupported target type: " + target);
        }
    
        // Load the arguments and types
        System.out.println("Load arguments and types");
        Class<?>[] argTypes = new Class<?>[args.length];
        for (int i = 0; i < args.length; i++) {
            Object arg = args[i];

            argTypes[i] = (arg == null) ? Object.class : getPrimitiveClass(arg.getClass());

            System.out.println("  argument: " + arg);
            if (arg instanceof Object[]) {
                Class<?> returnClass = processInstructions(mv, (Object[]) arg);
                argTypes[i] = getPrimitiveClass(returnClass);   // process returnClass
            } else if (arg instanceof Integer) {
                mv.visitLdcInsn(arg);
            } else if (arg instanceof String) {
                mv.visitLdcInsn(arg);
            } else if (arg instanceof Class<?>) {
                mv.visitLdcInsn(org.objectweb.asm.Type.getType((Class<?>) arg));
            } else {
                throw new IllegalArgumentException("Unsupported argument type: " + arg.getClass());
            }

            System.out.println("  type " + i + ": " + argTypes[i]);
        }
    
        if ( isReturn ) {
            mv.visitInsn(Opcodes.ARETURN);      // returns an Object
            return targetClass;  // Class of the result
        }

        // Fetch the method descriptor
        String methodName = (String) data[1];
        Method method = targetClass.getMethod(methodName, argTypes);

        System.out.println("call class.method: " + targetClass + " . " + methodName);
        String descriptor = org.objectweb.asm.Type.getMethodDescriptor(method);
    

        // Invoke the method
        if ( targetIsInstance ) {
            System.out.println("invoke virtual");
            mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, targetClass.getName().replace('.', '/'), methodName, descriptor, false);
        } else {
            System.out.println("invoke static");
            mv.visitMethodInsn(Opcodes.INVOKESTATIC, targetClass.getName().replace('.', '/'), methodName, descriptor, false);
        }

        Class<?> returnType = method.getReturnType();
        System.out.println("return type: " + returnType);
        return returnType;  // Class of the result
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
            Runtime mathOps = new Runtime(1);

            Object[][] data = {
                // { Integer.class, "new", 5 },     // calling a constructor with "new"
                // { System.out, "println", new Object[]{mathOps, "add", 5, 3} },
                // { System.out, "println", new Object[]{ Runtime.class, "add", 5, 3 } },
                { Runtime.class, "make", 5 },
                { Runtime.class, "print", 789 },
                { Runtime.class, "print", new Object[]{ Runtime.class, "make", 5 } },
                { Runtime.class, "print", new Object[]{"ARG", 0, Runtime.class} },  // use the argument
                { System.out, "println", "123" },
                { new Object[]{ Runtime.class, "make", 5 }, "add", 5 },
                // { System.out, "println", new Object[]{ new Object[]{"ARG", 0, Runtime.class}, "add", 5 }},                // call a method in the argument
                { new Object[]{"ARG", 0, Runtime.class}, "add", 5 },                // call a method in the argument
                { "RETURN", null, new Object[]{ Runtime.class, "make", 5 } }        // RETURN is optional in the end
            };

            // TODO - calling constructor with "new"

            // TODO - create multiple classes; ensure GC works for these classes

            // TODO - "env" access
            //          mv.visitFieldInsn(Opcodes.PUTFIELD, "GeneratedClass", "env", "I"); // Set the field

            /* TODO - statements like if/else

                Label elseLabel = new Label();
                Label endLabel = new Label();

                // Generate code for the condition
                condition.generateCode(mv);
                // Assuming the condition leaves a boolean on the stack
                mv.visitJumpInsn(IFEQ, elseLabel);

                // Generate code for the if block
                ifBlock.generateCode(mv);
                mv.visitJumpInsn(GOTO, endLabel);

                // Generate code for the else block
                mv.visitLabel(elseLabel);
                if (elseBlock != null) {
                    elseBlock.generateCode(mv);
                }

                // End of the if/else structure
                mv.visitLabel(endLabel);

            */

            // Create the class
            System.out.println("createClassWithMethod");
            byte[] classData = createClassWithMethod(data);
            CustomClassLoader loader = new CustomClassLoader();
            Class<?> generatedClass = loader.defineClass("GeneratedClass", classData);

            // Print debug statement
            System.out.println("Generated class: " + generatedClass.getName());

            // Call the generated method using reflection
            System.out.println("generatedClass.getMethod");
            Method method = generatedClass.getMethod("generatedMethod", Runtime.class);

            String descriptor = org.objectweb.asm.Type.getMethodDescriptor(method);
            Class<?> returnType = method.getReturnType();
            System.out.println("method descriptor: " + descriptor + " return type: " + returnType);

            System.out.println("invoke");
            // method.invoke(null);    // println returns void
            Object result = method.invoke(null, new Runtime(999));

            // Print the result
            System.out.println("Result of generatedMethod: " + result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}


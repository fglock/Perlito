import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Label;

import java.lang.reflect.Method;

public class ASMMethodCreator implements Opcodes {

    public static byte[] createClassWithMethod(String className, Object[][] env, Object[][] data) throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(V1_8, ACC_PUBLIC, className, null, "java/lang/Object", null);

        // Add static fields to the class (closed variables)
        for (int i = 0; i < env.length; i++) {
            String fieldName = (String)env[i][0];
            System.out.println("Create static field: " + fieldName);
            cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, fieldName, "LRuntime;", null, null).visitEnd();
        }

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

        generateCodeBlock(mv, data);    // Process the input data

        System.out.println("Return the last value");
        // mv.visitInsn(Opcodes.RETURN);   // returns void
        mv.visitInsn(Opcodes.ARETURN);      // returns an Object

        mv.visitMaxs(0, 0);                 // Max stack and local variables
        mv.visitEnd();
        cw.visitEnd();
        return cw.toByteArray();
    }

    public static void generateCodeBlock(MethodVisitor mv, Object[][] data) throws Exception {
        System.out.println("generateCodeBlock start");
        for (int i = 0; i < data.length; i++) {
            System.out.println("Process the input data line: "+i);
            processInstructions(mv, data[i]);
        }
        System.out.println("generateCodeBlock end");
    }

    private static Class<?> processInstructions(MethodVisitor mv, Object[] data) throws Exception {
        // if (data.length < 2) {
        //     throw new IllegalArgumentException("Invalid data structure");
        // }
    
        Object target = data[0];
    
        boolean targetIsInstance = true;
        boolean isReturn = false;
        Class<?> targetClass;

        // Load the target object
        System.out.println("Load the target object " + data[0]);
        System.out.println("         method        " + data[1] + " ... ");
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

            if ( target.equals("ARG")) {                // { "ARG", 0, int.class }   { ARG, index, type }
                mv.visitVarInsn(ALOAD, (int)(data[1]));
                return (Class<?>)(data[2]);   // return Class
            } else if ( target.equals("GETSTATIC")) {      // { "GETSTATIC", "env" }   { GETSTATIC, name }
                mv.visitFieldInsn(Opcodes.GETSTATIC, "Runtime", (String)data[1], "LRuntime;");
                return Runtime.class;   // return Class
            } else if ( target.equals("RETURN")) {      // { "RETURN", null, new Object[]{ Runtime.class, "make", 5 } }
                System.out.println(" calling return");
                targetClass = Runtime.class;
                isReturn = true;
            } else if ( target.equals("IF")) {      // { "IF", null, cond, if, else }
                System.out.println("IF start");
                Label elseLabel = new Label();
                Label endLabel = new Label();
                generateCodeBlock(mv, (Object[][])data[2]);  // Generate code for the condition
                mv.visitJumpInsn(IFEQ, elseLabel);  // Assuming the condition leaves a boolean on the stack
                generateCodeBlock(mv, (Object[][])data[3]);  // Generate code for the if block
                mv.visitJumpInsn(GOTO, endLabel);
                mv.visitLabel(elseLabel);
                if (data[4] != null) {            // Generate code for the else block
                    generateCodeBlock(mv, (Object[][])data[4]);
                }
                mv.visitLabel(endLabel);            // End of the if/else structure
                // targetClass = Runtime.class;
                System.out.println("IF end");
                return Runtime.class;  // Class of the result
            } else {
                throw new IllegalArgumentException("Unsupported target type: " + target);
                // targetClass = target.getClass();
                // mv.visitLdcInsn(target);
            }
        // } else if (target instanceof Integer) {
        //     System.out.println(" is Integer");
        //     targetClass = target.getClass();
        //     mv.visitLdcInsn(target);
        } else if (target instanceof java.io.PrintStream) {
            System.out.println(" is " + target);
            targetClass = target.getClass();
            mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        } else {
            throw new IllegalArgumentException("Unsupported target type: " + target);
        }
    
        Class<?>[] argTypes = {};
        boolean hasArguments = false;
        if ( data.length > 2 ) {
            // Load the arguments and types
            hasArguments = true;
            System.out.println("Load arguments and types");

            Object[] args = new Object[data.length - 2];
            System.arraycopy(data, 2, args, 0, args.length);

            argTypes = new Class<?>[args.length];
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
        } else {
            System.out.println("no arguments");
        }
    
        if ( isReturn ) {
            mv.visitInsn(Opcodes.ARETURN);      // returns an Object
            return targetClass;  // Class of the result
        }

        // Fetch the method descriptor
        String methodName = (String) data[1];
        Method method = hasArguments ? targetClass.getMethod(methodName, argTypes)
            : targetClass.getMethod(methodName);
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

            // TODO - calling constructor with "new"

            // TODO - create multiple classes; ensure GC works for these classes

            // TODO - "env" access
            // {         "GETSTATIC", "env" },     // TODO retrieve closed variable
            //          mv.visitFieldInsn(Opcodes.PUTFIELD, "GeneratedClass", "env", "I"); // Set the field

            Runtime mathOps = new Runtime(111);
            String className = "GeneratedClass";

            // Create the class
            System.out.println("createClassWithMethod");
            byte[] classData = createClassWithMethod(
                className,
                new Object[][]{     // closed variables  { name, initial value }
                    { "env", mathOps },
                },
                new Object[][]{
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

                    { "IF", null,
                        new Object[][]{ { Runtime.class, "is_false" } },      // if condition
                        new Object[][]{ { Runtime.class, "print", "if is true" } },    // if block
                        new Object[][]{ { Runtime.class, "print", "if is false" } },    // else block
                    },

                    // { "GETSTATIC", "env" },     // TODO retrieve closed variable
                    // { Runtime.class, "print", new Object[]{ "GETSTATIC", "env" } },

                    { "RETURN", null, new Object[]{ Runtime.class, "make", 5 } }        // RETURN is optional in the end
                }
            );

            CustomClassLoader loader = new CustomClassLoader();
            Class<?> generatedClass = loader.defineClass(className, classData);

            // Print debug statement
            System.out.println("Generated class: " + generatedClass.getName());

            // Set the static field value
            // myClass.getField("myStaticVar").setInt(null, 42);

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


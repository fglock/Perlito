import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Label;

import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.util.concurrent.Callable;

public class ASMMethodCreator implements Opcodes {

    static int classCounter = 0;

    public static Class<?> createClassWithMethod(
        Object[][] env, 
        Object[][] lexicals,
        Object[][] data
    ) throws Exception {
        // Create a ClassWriter with COMPUTE_FRAMES and COMPUTE_MAXS options
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);

        String classNameDot = "org.perlito.anon" + String.valueOf(classCounter++);
        String className = classNameDot.replace('.', '/');

        // Define the class with version, access flags, name, signature, superclass, and interfaces
        // the class implements Callable
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, className, null, "java/lang/Object", new String[]{"java/util/concurrent/Callable"});

        // Add a private instance field to store the call() argument
        cw.visitField(Opcodes.ACC_PRIVATE, "arg", "LRuntime;", null, null).visitEnd();

        // Add static fields to the class (closed variables)
        for (int i = 0; i < env.length; i++) {
            String fieldName = (String)env[i][0];
            System.out.println("Create static field: " + fieldName);
            cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, fieldName, "LRuntime;", null, null).visitEnd();
        }

        // Add instance fields to the class (lexical variables)
        for (int i = 0; i < lexicals.length; i++) {
            String fieldName = (String)lexicals[i][0];
            System.out.println("Create static field: " + fieldName);
            cw.visitField(Opcodes.ACC_PUBLIC, fieldName, "LRuntime;", null, null).visitEnd();
        }


        // Create the class initializer method
        mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
        mv.visitCode();
        // Initialize the static fields
        // TODO
        // // mv.visitInsn(Opcodes.ICONST_5); // Push the constant value 5 onto the stack
        // // mv.visitFieldInsn(Opcodes.PUTSTATIC, className, "staticField", "I"); // Set the static field
        // Return from the method
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(1, 0);
        mv.visitEnd();


        // Add a constructor that accepts a Runtime parameter
        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "(LRuntime;)V", null, null);
        mv.visitCode();
        mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
        mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false); // Call the superclass constructor
        mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
        mv.visitVarInsn(Opcodes.ALOAD, 1); // Load the constructor parameter
        mv.visitFieldInsn(Opcodes.PUTFIELD, className, "arg", "LRuntime;"); // Store the parameter in the field
        mv.visitInsn(Opcodes.RETURN); // Return void
        mv.visitMaxs(0, 0); // Automatically computed
        mv.visitEnd();

        // Create the method
        System.out.println("Create the method");
        String return_type = "()Ljava/lang/Object;";       // Callable takes no arguments, returns an Object
        // String return_type = "()Ljava/lang/Object;";    // returns an Object
        // String return_type = "()V";                     // returns void

        // method is public, instance method
        mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "call", return_type, null, new String[]{"java/lang/Exception"});
        mv.visitCode();

        Label returnLabel = new Label();

        generateCodeBlock(mv, className, data, returnLabel);    // Process the input data

        System.out.println("Return the last value");
        mv.visitLabel(returnLabel);
        // mv.visitInsn(Opcodes.RETURN);   // returns void
        mv.visitInsn(Opcodes.ARETURN);      // returns an Object

        mv.visitMaxs(0, 0);                 // max stack and local variables
        mv.visitEnd();
        cw.visitEnd();                      // complete the class
        byte[] classData = cw.toByteArray();    // generate the bytecode

        CustomClassLoader loader = new CustomClassLoader();
        Class<?> generatedClass = loader.defineClass(classNameDot, classData);  // generate the class
        return generatedClass;
    }

    public static void generateCodeBlock(
        MethodVisitor mv, 
        String className, 
        Object[][] data,
        Label returnLabel
    ) throws Exception {
        System.out.println("generateCodeBlock start");
        for (int i = 0; i < data.length; i++) {
            System.out.println("Process the input data line: "+i);
            processInstructions(mv, className, data[i], returnLabel);
        }
        System.out.println("generateCodeBlock end");
    }

    private static Class<?> processInstructions(MethodVisitor mv, String className, Object[] data, Label returnLabel) throws Exception {
        // if (data.length < 2) {
        //     throw new IllegalArgumentException("Invalid data structure");
        // }
    
        Object target = data[0];
    
        boolean targetIsInstance = true;
        boolean isReturn = false;
        Class<?> targetClass;

        // Load the target object
        System.out.println("Load the target object " + data[0]);
        // System.out.println("         method        " + data[1] + " ... ");
        if (target instanceof Object[]) {
            //  { new Object[]{ Runtime.class, "make", 5 }, "add", 5 },
            targetClass = processInstructions(mv, className, (Object[]) target, returnLabel);
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
        } else if (target instanceof String) {
            System.out.println(" is String");

            if ( target.equals("ARG")) {                // { "ARG" }
                System.out.println("retrieve field " + "arg");
                mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
                mv.visitFieldInsn(Opcodes.GETFIELD, className, "arg", "LRuntime;"); // Load the field value
                return Runtime.class;   // return Class
            } else if ( target.equals("GETSTATIC")) {      // { "GETSTATIC", "env" }   { GETSTATIC, name }
                System.out.println("retrieve static " + (String)data[1]);
                mv.visitFieldInsn(Opcodes.GETSTATIC, className, (String)data[1], "LRuntime;");
                return Runtime.class;   // return Class
            } else if ( target.equals("GETFIELD")) {      // { "GETFIELD", "var" }   { GETFIELD, name }
                System.out.println("retrieve field " + (String)data[1]);
                mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
                mv.visitFieldInsn(Opcodes.GETFIELD, className, (String)data[1], "LRuntime;");
                return Runtime.class;   // return Class
            } else if ( target.equals("PUTFIELD")) {      // { "PUTFIELD", "var" }   { PUTFIELD, name }
                System.out.println("put field " + (String)data[1]);
                // TODO process argument
                mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
                mv.visitFieldInsn(Opcodes.PUTFIELD, className, (String)data[1], "LRuntime;");
                return Runtime.class;   // return Class
            } else if ( target.equals("RETURN")) {      // { "RETURN", null, new Object[]{ Runtime.class, "make", 5 } }
                System.out.println(" calling return");
                targetClass = Runtime.class;
                isReturn = true;
            } else if ( target.equals("IF")) {      // { "IF", null, cond, if, else }
                System.out.println("IF start");
                Label elseLabel = new Label();
                Label endLabel = new Label();
                generateCodeBlock(mv, className, (Object[][])data[2], returnLabel);  // Generate code for the condition
                mv.visitJumpInsn(IFEQ, elseLabel);  // Assuming the condition leaves a boolean on the stack
                generateCodeBlock(mv, className, (Object[][])data[3], returnLabel);  // Generate code for the if block
                mv.visitJumpInsn(GOTO, endLabel);
                mv.visitLabel(elseLabel);
                if (data[4] != null) {            // Generate code for the else block
                    generateCodeBlock(mv, className, (Object[][])data[4], returnLabel);
                }
                mv.visitLabel(endLabel);            // End of the if/else structure
                // targetClass = Runtime.class;
                System.out.println("IF end");
                return Runtime.class;  // Class of the result
            } else if ( target.equals("MY")) {      // { "MY", "$a" }
                System.out.println("MY " + data[1]);
                // TODO set in the scope/frame

                return Runtime.class;  // Class of the result
            } else if ( target.equals("SUB")) {      // { "SUB", className, env, lexicals, body }
                System.out.println("SUB start");
                Object[][] newEnv  = (Object[][])data[1];    // env
                Object[][] newLexicals = (Object[][])data[2];    // lexicals
                Object[][] newData = (Object[][])data[3];    // data

                Class<?> generatedClass = createClassWithMethod(newEnv, newLexicals, newData);
                generatedClass.getField("env").set(null, new Runtime(111));     // TODO set static field value

                // save the class in a public place
                String newClassName = "org.perlito.anon" + String.valueOf(classCounter++);
                Runtime.anonSubs.put(newClassName, generatedClass);

                // Runtime.make_sub(className);
                mv.visitLdcInsn(newClassName);
                mv.visitMethodInsn(Opcodes.INVOKESTATIC, "Runtime", "make_sub", "(Ljava/lang/String;)LRuntime;", false);
                System.out.println("SUB end");
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
                    Class<?> returnClass = processInstructions(mv, className, (Object[]) arg, returnLabel);
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
            // mv.visitInsn(Opcodes.ARETURN);      // returns an Object
            mv.visitJumpInsn(GOTO, returnLabel);
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

            //      - when something is called in void context, we need to POP the JVM stack
            //        to cleanup the unused value

            // TODO - implement thread-safety - it may need locking when calling ASM

            // TODO - calling constructor with "new"

            // TODO - create multiple classes; ensure GC works for these classes

            // TODO - "env" access - create a method to initialize the static field values, instead of using reflection
            //          generatedClass.getField("env").set(null, mathOps);
            //          use "clinit" method

            //      - lexical variables like "my"
            //          GETFIELD, PUTFIELD
            //          initialize variables

            //      - goto, macros - control structures
            //      - implement "local"
            //      - eval, BEGIN-block
            //      - subroutine declaration
            //          create a Runtime.call(arg) method

            // Create the class
            System.out.println("createClassWithMethod");
            Class<?> generatedClass = createClassWithMethod(
                new Object[][]{     // closed variables  { name }
                    { "env" },
                },
                new Object[][]{     // lexical variables  { name }
                    { "var" },
                },
                new Object[][]{
                    // { Integer.class, "new", 5 },     // calling a constructor with "new"
                    // { System.out, "println", new Object[]{ Runtime.class, "add", 5, 3 } },
                    { Runtime.class, "make", 5 },
                    { Runtime.class, "print", 789 },
                    { "ARG" },          // retrieve the argument
                    { Runtime.class, "print", new Object[]{ Runtime.class, "make", 5 } },
                    { Runtime.class, "print", new Object[]{ "ARG" } },  // use the argument
                    { System.out, "println", "123" },
                    { new Object[]{ Runtime.class, "make", 5 }, "add", 5 },
                    // { System.out, "println", new Object[]{ new Object[]{ "ARG" }, "add", 5 }},                // call a method in the argument
                    { new Object[]{ "ARG" }, "add", 5 },                // call a method in the argument

                    { "IF", null,
                        new Object[][]{ { Runtime.class, "is_false" } },      // if condition
                        new Object[][]{ { Runtime.class, "print", "if is true" } },    // if block
                        new Object[][]{ { Runtime.class, "print", "if is false" } },    // else block
                    },

                    { Runtime.class, "print", new Object[]{ "GETSTATIC", "env" } },  // retrieve closed variable

                    { new Object[]{ "SUB",
                        new Object[][]{     // closed variables  { name }
                            { "env" },
                        },
                        new Object[][]{     // lexical variables  { name }
                            { "var2" },
                        },
                        new Object[][]{
                            { Runtime.class, "print", new Object[]{ "ARG" } },
                        }
                    }, "apply", new Object[]{ Runtime.class, "make", 55555 } },

                    // { "MY", "$a" },

                    { "RETURN", null, new Object[]{ Runtime.class, "make", 5 } }        // RETURN is optional at the end
                }
            );

            System.out.println("Generated class: " + generatedClass.getName());

            // Set the static field value
            // TODO - move to class definition; create initializer method
            generatedClass.getField("env").set(null, new Runtime(111));

            // TODO "lexical variable"
            // generatedClass.getField("var").set(null, new Runtime(222));     // "lexical variable"

            // Create an instance of the class with argument "new Runtime(999)" and call the call() method
            Callable<?> callableInstance = (Callable<?>) generatedClass.getDeclaredConstructor(Runtime.class).newInstance(new Runtime(999));
            System.out.println("call");
            Runtime result = (Runtime) callableInstance.call();

            // Print the result
            System.out.println("Result of generatedMethod: " + result);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}


import java.util.*;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

/**
 * ASMMethodCreator is a utility class that uses the ASM library to dynamically generate Java classes
 * with specific methods. It is designed to create classes with methods that can be used for runtime
 * evaluation of expressions or statements in a simulated Perl environment.
 */
public class ASMMethodCreator implements Opcodes {

    // Counter for generating unique class names
    static int classCounter = 0;

    // Custom class loader to load generated classes
    static CustomClassLoader loader = new CustomClassLoader();

    /**
     * Creates a new class with a method based on the provided context, environment, and abstract
     * syntax tree (AST).
     *
     * @param ctx The emitter context containing information for code generation.
     * @param env An array of environment variable names to be included as static fields in the class.
     * @param ast The abstract syntax tree representing the method body.
     * @return The generated class.
     * @throws Exception If an error occurs during class creation.
     */
    public static Class<?> createClassWithMethod(EmitterContext ctx, String[] env, Node ast) throws Exception {
        // Create a ClassWriter with COMPUTE_FRAMES and COMPUTE_MAXS options for automatic frame and max stack size calculation
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);

        // Generate a unique class name
        String javaClassNameDot = "org.perlito.anon" + classCounter++;
        String javaClassName = javaClassNameDot.replace('.', '/');

        // Set the source file name for runtime error messages
        cw.visitSource(ctx.fileName, null);

        // Define the class with version, access flags, name, signature, superclass, and interfaces
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, javaClassName, null, "java/lang/Object", null);

        // Add static fields to the class for closure variables
        for (String fieldName : env) {
            System.out.println("Create static field: " + fieldName);
            cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, fieldName, "LRuntime;", null, null).visitEnd();
        }

        // Set the context type and other context properties
        ctx.contextType = ContextType.RUNTIME;
        ctx.javaClassName = javaClassName;
        ctx.isBoxed = true;

        // Create the class initializer method
        ctx.mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
        ctx.mv.visitCode();
        // The static initializer is currently empty. If static field initialization is needed, uncomment the following block.
        /*
        for (int i = 0; i < env.length; i++) { // Initialize the static fields
            String fieldName = env[i];
            System.out.println("Init static field: " + fieldName);
            ctx.mv.visitTypeInsn(Opcodes.NEW, "Runtime");
            ctx.mv.visitInsn(Opcodes.DUP);
            ctx.mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "Runtime", "<init>", "()V", false); // Create a new instance of Runtime
            ctx.mv.visitFieldInsn(Opcodes.PUTSTATIC, javaClassName, fieldName, "LRuntime;"); // Set the static field
        }
        */
        ctx.mv.visitInsn(Opcodes.RETURN);
        ctx.mv.visitMaxs(0, 0);
        ctx.mv.visitEnd();

        // Add a constructor without parameters
        ctx.mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
        ctx.mv.visitCode();
        ctx.mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
        ctx.mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false); // Call the superclass constructor
        ctx.mv.visitInsn(Opcodes.RETURN); // Return void
        ctx.mv.visitMaxs(0, 0); // Automatically computed
        ctx.mv.visitEnd();

        // Create the main method for the generated class
        System.out.println("Create the method");
        String returnType = "(LRuntime;LContextType;)Ljava/lang/Object;";

        // Define the method as public and static
        ctx.mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "apply", returnType, null, new String[] {"java/lang/Exception"});

        // Generate the subroutine block
        ctx.mv.visitCode();

        // Initialize local variables with closure values from static fields
        // Skip indices 0 and 1 because they are reserved for special arguments ("@_" and call context)
        for (int i = 2; i < env.length; i++) {
            String fieldName = env[i];
            System.out.println("Init closure variable: " + fieldName);
            ctx.mv.visitFieldInsn(Opcodes.GETSTATIC, javaClassName, fieldName, "LRuntime;");
            ctx.mv.visitVarInsn(Opcodes.ASTORE, i);
        }

        // Create a label for the return point
        ctx.returnLabel = new Label();

        // Visit the AST to generate bytecode
        EmitterVisitor visitor = new EmitterVisitor(ctx);
        ast.accept(visitor);

        // Handle the return value
        System.out.println("Return the last value");
        ctx.mv.visitLabel(ctx.returnLabel); // "return" from other places arrive here
        ctx.mv.visitInsn(Opcodes.ARETURN); // returns an Object
        ctx.mv.visitMaxs(0, 0); // Automatically computed
        ctx.mv.visitEnd();

        // Complete the class
        cw.visitEnd();
        byte[] classData = cw.toByteArray(); // Generate the bytecode

        // Define the class using the custom class loader
        return loader.defineClass(javaClassNameDot, classData);
    }
}


import java.util.*;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

public class ASMMethodCreator implements Opcodes {

  static int classCounter = 0;
  static CustomClassLoader loader = new CustomClassLoader();

  public static Class<?> createClassWithMethod(EmitterContext ctx, String[] env, Node ast)
      throws Exception {
    // Create a ClassWriter with COMPUTE_FRAMES and COMPUTE_MAXS options
    ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);

    String javaClassNameDot = "org.perlito.anon" + String.valueOf(classCounter++);
    String javaClassName = javaClassNameDot.replace('.', '/');

    // Set the source file name for runtime error messages
    cw.visitSource(ctx.fileName, null);

    // Define the class with version, access flags, name, signature, superclass, and interfaces
    cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, javaClassName, null, "java/lang/Object", null);

    // Add static fields to the class
    // closure variables are stored here; they are copied to local vars at runtime
    for (int i = 0; i < env.length; i++) {
      String fieldName = env[i];
      System.out.println("Create static field: " + fieldName);
      cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, fieldName, "LRuntime;", null, null)
          .visitEnd();
    }

    ctx.contextType = ContextType.RUNTIME;
    ctx.javaClassName = javaClassName;
    ctx.isBoxed = true;

    // Create the class initializer method
    ctx.mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
    ctx.mv.visitCode();
    // for (int i = 0; i < env.length; i++) { // Initialize the static fields
    //   String fieldName = (String) env[i];
    //   System.out.println("Init static field: " + fieldName);
    //   ctx.mv.visitTypeInsn(Opcodes.NEW, "Runtime");
    //   ctx.mv.visitInsn(Opcodes.DUP);
    //   ctx.mv.visitMethodInsn(
    //       Opcodes.INVOKESPECIAL,
    //       "Runtime",
    //       "<init>",
    //       "()V",
    //       false); // Create a new instance of Runtime
    //   ctx.mv.visitFieldInsn(
    //       Opcodes.PUTSTATIC, javaClassName, fieldName, "LRuntime;"); // Set the static field
    // }
    ctx.mv.visitInsn(Opcodes.RETURN);
    ctx.mv.visitMaxs(0, 0);
    ctx.mv.visitEnd();

    // Add a constructor without parameters
    ctx.mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
    ctx.mv.visitCode();
    ctx.mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
    ctx.mv.visitMethodInsn(
        Opcodes.INVOKESPECIAL,
        "java/lang/Object",
        "<init>",
        "()V",
        false); // Call the superclass constructor
    ctx.mv.visitInsn(Opcodes.RETURN); // Return void
    ctx.mv.visitMaxs(0, 0); // Automatically computed
    ctx.mv.visitEnd();

    // Create the method
    System.out.println("Create the method");
    // takes a "Runtime" arg "@_" and a ContextType, returns a "Runtime"
    String return_type = "(LRuntime;LContextType;)Ljava/lang/Object;";

    // method is public, static method
    ctx.mv =
        cw.visitMethod(
            Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
            "apply",
            return_type,
            null,
            new String[] {"java/lang/Exception"});

    // generate the subroutine block
    ctx.mv.visitCode();

    // initialize local variables with the closure values from the static fields
    // skip 0 and 1 because they are the "@_" argument list and the call context
    for (int i = 2; i < env.length; i++) {
      String fieldName = env[i];
      System.out.println("Init closure variable: " + fieldName);
      ctx.mv.visitFieldInsn(Opcodes.GETSTATIC, javaClassName, fieldName, "LRuntime;");
      ctx.mv.visitVarInsn(Opcodes.ASTORE, i);
    }

    ctx.returnLabel = new Label();

    EmitterVisitor visitor = new EmitterVisitor(ctx);
    ast.accept(visitor);

    System.out.println("Return the last value");
    ctx.mv.visitLabel(ctx.returnLabel); // "return" from other places arrive here
    ctx.mv.visitInsn(Opcodes.ARETURN); // returns an Object
    ctx.mv.visitMaxs(0, 0); // max stack and local variables
    ctx.mv.visitEnd();

    cw.visitEnd(); // complete the class
    byte[] classData = cw.toByteArray(); // generate the bytecode

    Class<?> generatedClass = loader.defineClass(javaClassNameDot, classData); // generate the class
    return generatedClass;
  }
}


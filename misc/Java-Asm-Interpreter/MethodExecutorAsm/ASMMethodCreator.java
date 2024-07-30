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

  public static void main(String[] args) {
    try {

      String code =
          ""
              + "my $a = 15 ;"
              + "my $x = $a ;"
              + "print $x ;"
              + "$a = 12 ;"
              + "print $a ;"
              + " ( sub { print @_ } )->(88888) ;"
              + "print $a ;"
              + "do { $a; if (1) { print 123 } elsif (3) { print 345 } else { print 456 } } ;"
              + "return 5;";
      if (args.length >= 2 && args[0].equals("-e")) {
        code = args[1]; // Read the code from the command line parameter
      }

      // Create the compiler context
      EmitterContext ctx =
          new EmitterContext(
              "test.pl", // source filename
              null, // java class name
              new ScopedSymbolTable(), // top-level ctx.symbolTable
              null, // return label
              null, // method visitor
              null, // call context
              false // is boxed
              );
      ctx.symbolTable.enterScope();
      ctx.symbolTable.addVariable("@_"); // argument is local variable 0
      ctx.symbolTable.addVariable("wantarray"); // call context is local variable 1

      // Parse the code
      System.out.println("parse code: " + code);
      System.out.println("  call context " + ctx.contextType);
      Lexer lexer = new Lexer(code);
      List<Token> tokens = lexer.tokenize();
      Parser parser = new Parser(tokens);
      Node ast = parser.parse();
      System.out.println("-- AST:\n" + Parser.getASTString(ast) + "--\n");

      // Create the class
      System.out.println("createClassWithMethod");
      Class<?> generatedClass =
          createClassWithMethod(
              ctx,
              new String[] {}, // closure variables  { name }
              ast);

      // Convert into a Runtime object
      String newClassName = generatedClass.getName();
      Runtime.anonSubs.put(newClassName, generatedClass);
      Runtime anonSub = Runtime.make_sub(newClassName);
      Runtime result = anonSub.apply(new Runtime(999), ContextType.SCALAR);

      System.out.println("Result of generatedMethod: " + result);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}

/* TODO

  - easy wins
        - loops
        - string

  - harder
        - BEGIN block
        - eval string
        - eval block, catch error

  - Parser: low-precedence operators not, or, and

  - cleanup the closure code to only add the lexical variables mentioned in the AST

  - format error messages and warnings
        - compile time: get file position from lexer
        - run-time: add annotations to the bytecode

  - test different Perl data types
        - array, hash, string, double, references
        - experiment with Perlito runtime

  - global variables and namespaces
        - named subroutine declaration
        - Perl classes

  - local variables
      set up the cleanup before RETURN
      set up exception handling

  - add debug information (line numbers)
      Label thisLabel = new Label();
      ctx.mv.visitLabel(thisLabel);
      ctx.mv.visitLineNumber(10, thisLabel); // Associate line number 10 with thisLabel

  - tests

  - implement thread-safety - it may need locking when calling ASM

  - create multiple classes; ensure GC works for these classes

  - goto, macros - control structures
        - test FOR, WHILE

  - eval string
        freeze the ctx.symbolTable at eval string, we will need it to compile the string later

  - BEGIN-block

  - read code from STDIN

        // Read input from STDIN
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter code:");
        String code = scanner.nextLine();
        scanner.close();

*/

import java.lang.reflect.Method;
import java.util.*;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

public class ASMMethodCreator implements Opcodes {

  static int classCounter = 0;
  static CustomClassLoader loader = new CustomClassLoader();

  public static Class<?> createClassWithMethod(EmitterContext ctx, String[] env, Object[][] data)
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
    generateCodeBlock(ctx, data); // Process the input data
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

  public static void generateCodeBlock(EmitterContext ctx, Object[][] data) throws Exception {
    System.out.println("generateCodeBlock start");
    ctx.symbolTable.enterScope();

    EmitterContext ctxMiddle =
        ctx.with(ContextType.VOID); // statements in the middle of the block have context VOID

    for (int i = 0; i < data.length; i++) {
      System.out.println("Process the input data line: " + i);
      processInstructions(
          i == (data.length - 1) ? ctx : ctxMiddle, // void context, except for the last line
          data[i]);
    }
    ctx.symbolTable.exitScope();
    System.out.println("generateCodeBlock end");
  }

  private static Class<?> processInstructions(EmitterContext ctx, Object[] data) throws Exception {

    Object target = data[0]; // Load the target object
    boolean targetIsInstance = true;
    Class<?> targetClass;

    System.out.println("processInstructions " + data[0] + " context: " + ctx.contextType);
    if (target instanceof Object[]) { //  { new Object[]{ Runtime.class, "make", 5 }, "add", 5 },
      targetClass = processInstructions(ctx, (Object[]) target);
      System.out.println(" target is instance of: " + targetClass);
    } else if (target instanceof Class<?>) {
      targetIsInstance = false; // If the target is a class, it means we're calling a static method
      targetClass = (Class<?>) target;
      System.out.println(" is Class");
      String methodName = (String) data[1];
      if (methodName.equals("new")) {
        // we are we calling a constructor
        System.out.println(" calling a constructor");
        // TODO
        //      ctx.mv.visitTypeInsn(Opcodes.NEW, "java/lang/Integer"); // Create a new Integer
        // object
        //      ctx.mv.visitInsn(Opcodes.DUP); // Duplicate the top operand stack value
        //      ctx.mv.visitVarInsn(Opcodes.ILOAD, 1); // Load the method argument (int value)
        //      ctx.mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V",
        // false); // Call the Integer constructor
        throw new Exception("Not implemented: calling a constructor");
      }
    } else if (target instanceof String) {
      System.out.println(" is String");

      if (target.equals("PARSE")) { // { "PARSE", "1+1" }
        String code = (String) data[1];
        System.out.println("parse code: " + code);
        System.out.println("  call context " + ctx.contextType);
        Lexer lexer = new Lexer(code);
        List<Token> tokens = lexer.tokenize();
        Parser parser = new Parser(tokens);
        Node ast = parser.parse();
        System.out.println("-- AST:\n" + Parser.getASTString(ast) + "--\n");
        EmitterVisitor visitor = new EmitterVisitor(ctx);
        ast.accept(visitor);
        return Runtime.class; // return Class
      } else if (target.equals("IF")) { // { "IF", cond, if, else }
        System.out.println("IF start");
        ctx.symbolTable.enterScope();
        Label elseLabel = new Label();
        Label endLabel = new Label();
        processInstructions(
            ctx.with(ContextType.SCALAR), (Object[]) data[1]); // Generate code for the condition
        ctx.mv.visitJumpInsn(
            IFEQ, elseLabel); // Assuming the condition leaves a boolean on the stack
        generateCodeBlock(ctx, (Object[][]) data[2]); // Generate code for the if block
        ctx.mv.visitJumpInsn(GOTO, endLabel);
        ctx.mv.visitLabel(elseLabel);
        if (data[3] != null) { // Generate code for the else block
          generateCodeBlock(ctx, (Object[][]) data[3]);
        }
        ctx.mv.visitLabel(endLabel); // End of the if/else structure
        ctx.symbolTable.exitScope();
        System.out.println("IF end");
        return Runtime.class; // Class of the result
      } else if (target.equals("WHILE")) { // { "WHILE", cond, body }
        System.out.println("WHILE start");
        ctx.symbolTable.enterScope();
        Label startLabel = new Label();
        Label endLabel = new Label();

        ctx.mv.visitLabel(startLabel);
        processInstructions(
            ctx.with(ContextType.SCALAR), (Object[]) data[1]); // Generate code for the condition
        ctx.mv.visitJumpInsn(
            IFEQ, endLabel); // Assuming the condition leaves a boolean on the stack
        generateCodeBlock(ctx, (Object[][]) data[2]); // Generate code for the loop body
        ctx.mv.visitJumpInsn(GOTO, startLabel); // Jump back to the start of the loop
        ctx.mv.visitLabel(endLabel); // End of the loop
        ctx.symbolTable.exitScope();
        System.out.println("WHILE end");
        return Runtime.class; // Class of the result
      } else if (target.equals("FOR")) { // { "FOR", init, cond, incr, body }
        System.out.println("FOR start");
        ctx.symbolTable.enterScope();
        Label startLabel = new Label();
        Label endLabel = new Label();

        processInstructions(ctx, (Object[]) data[1]); // Generate code for the initialization
        ctx.mv.visitLabel(startLabel);
        processInstructions(
            ctx.with(ContextType.SCALAR), (Object[]) data[2]); // Generate code for the condition
        ctx.mv.visitJumpInsn(
            IFEQ, endLabel); // Assuming the condition leaves a boolean on the stack
        generateCodeBlock(ctx, (Object[][]) data[4]); // Generate code for the loop body
        processInstructions(ctx, (Object[]) data[3]); // Generate code for the increment
        ctx.mv.visitJumpInsn(GOTO, startLabel); // Jump back to the start of the loop
        ctx.mv.visitLabel(endLabel); // End of the loop
        ctx.symbolTable.exitScope();
        System.out.println("FOR end");
        return Runtime.class; // Class of the result
      } else if (target.equals("SETVAR")) { // { "SETVAR", "$a", new Object[] { "PARSE", "12} },
        System.out.println("SETVAR " + data[1]);
        String var = (String) data[1];
        int varIndex = ctx.symbolTable.getVariableIndex(var);
        if (varIndex == -1) {
          System.out.println(
              "Warning: Global symbol \""
                  + var
                  + "\" requires explicit package name (did you forget to declare \"my "
                  + var
                  + "\"?)");
        }
        processInstructions(ctx.with(ContextType.SCALAR), (Object[]) data[2]);
        ctx.mv.visitVarInsn(Opcodes.ASTORE, varIndex);
        if (ctx.contextType != ContextType.VOID) {
          ctx.mv.visitVarInsn(Opcodes.ALOAD, varIndex); // return the variable
        }
        System.out.println("SETVAR end " + varIndex);
        return Runtime.class; // Class of the result
      } else if (target.equals("SUB")) { // { "SUB", javaClassName, env, body }
        System.out.println("SUB start");

        // retrieve closure variable list
        // alternately, scan the AST for variables and capture only the ones that are used
        Map<Integer, String> visibleVariables = ctx.symbolTable.getAllVisibleVariables();
        String[] newEnv = new String[visibleVariables.size()];
        System.out.println(" ctx.symbolTable.getAllVisibleVariables");
        for (Integer index : visibleVariables.keySet()) {
          String variableName = visibleVariables.get(index);
          System.out.println("  " + index + " " + variableName);
          newEnv[index] = variableName;
        }

        // create the new method
        EmitterContext subCtx =
            new EmitterContext(
                ctx.fileName, // same source filename
                null, // java class name
                ctx.symbolTable, // closure symbolTable
                null, // return label
                null, // method visitor
                null, // call context
                false // is boxed
                );
        Object[][] newData = (Object[][]) data[2]; // AST
        Class<?> generatedClass = createClassWithMethod(subCtx, newEnv, newData);
        String newClassNameDot = generatedClass.getName();
        String newClassName = newClassNameDot.replace('.', '/');
        System.out.println(
            "Generated class name: " + newClassNameDot + " internal " + newClassName);
        System.out.println("Generated class env:  " + newEnv);

        // initialize the static fields
        // skip 0 and 1 because they are the "@_" argument list and the call context
        for (int i = 2; i < newEnv.length; i++) {
          ctx.mv.visitVarInsn(Opcodes.ALOAD, i); // copy local variable to the new class
          ctx.mv.visitFieldInsn(PUTSTATIC, newClassName, newEnv[i], "LRuntime;");
        }

        // this will be called at runtime: Runtime.make_sub(javaClassName);
        // TODO move the "make_sub" to ASM
        Runtime.anonSubs.put(newClassName, generatedClass);
        ctx.mv.visitLdcInsn(newClassName);
        ctx.mv.visitMethodInsn(
            Opcodes.INVOKESTATIC, "Runtime", "make_sub", "(Ljava/lang/String;)LRuntime;", false);
        System.out.println("SUB end");
        return Runtime.class; // Class of the result
      } else {
        throw new IllegalArgumentException("Unsupported target type: " + target);
      }
    } else if (target instanceof java.io.PrintStream) {
      System.out.println(" is " + target);
      targetClass = target.getClass();
      ctx.mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
    } else {
      throw new IllegalArgumentException("Unsupported target type: " + target);
    }

    Class<?>[] argTypes = {};
    boolean hasArguments = false;
    if (data.length > 2) {
      // Load the arguments and types
      hasArguments = true;
      System.out.println("Load arguments and types");

      Object[] args = new Object[data.length - 2];
      System.arraycopy(data, 2, args, 0, args.length);
      EmitterContext ctxScalar = ctx.with(ContextType.SCALAR);

      argTypes = new Class<?>[args.length];
      for (int i = 0; i < args.length; i++) {
        Object arg = args[i];
        argTypes[i] = (arg == null) ? Object.class : getPrimitiveClass(arg.getClass());
        System.out.println("  argument: " + arg);
        if (arg instanceof Object[]) {
          Class<?> returnClass = processInstructions(ctxScalar, (Object[]) arg);
          argTypes[i] = getPrimitiveClass(returnClass); // process returnClass
        } else if (arg instanceof Integer) {
          ctxScalar.mv.visitLdcInsn(arg);
        } else if (arg instanceof String) {
          ctxScalar.mv.visitLdcInsn(arg);
        } else if (arg instanceof Class<?>) {
          ctxScalar.mv.visitLdcInsn(org.objectweb.asm.Type.getType((Class<?>) arg));
        } else if (arg instanceof ContextType) {
            if ( (ContextType) arg == ContextType.SCALAR ) {
                // Load the ContextType.SCALAR enum constant onto the stack
                ctxScalar.mv.visitFieldInsn(GETSTATIC, "ContextType", "SCALAR", "LContextType;");
            }
        } else {
          throw new IllegalArgumentException("Unsupported argument type: " + arg.getClass());
        }
        System.out.println("  type " + i + ": " + argTypes[i]);
      }
    } else {
      System.out.println("no arguments");
    }

    // Fetch the method descriptor
    String methodName = (String) data[1];
    Method method =
        hasArguments
            ? targetClass.getMethod(methodName, argTypes)
            : targetClass.getMethod(methodName);
    String descriptor = org.objectweb.asm.Type.getMethodDescriptor(method);
    System.out.println(
        "call class.method: "
            + targetClass
            + " . "
            + methodName
            + " descriptor: "
            + descriptor
            + " context: "
            + ctx.contextType);

    // Invoke the method
    if (targetIsInstance) {
      System.out.println("invoke virtual");
      ctx.mv.visitMethodInsn(
          Opcodes.INVOKEVIRTUAL,
          targetClass.getName().replace('.', '/'),
          methodName,
          descriptor,
          false);
    } else {
      System.out.println("invoke static");
      ctx.mv.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          targetClass.getName().replace('.', '/'),
          methodName,
          descriptor,
          false);
    }
    if (ctx.contextType == ContextType.VOID && descriptor.charAt(descriptor.length() - 1) != 'V') {
      System.out.println(" in void context");
      ctx.mv.visitInsn(Opcodes.POP); // cleanup the stack
    }
    Class<?> returnType = method.getReturnType();
    System.out.println("return type: " + returnType);
    return returnType; // Class of the result
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
      ctx.symbolTable.addVariable("@_");        // argument is local variable 0
      ctx.symbolTable.addVariable("wantarray"); // call context is local variable 1

      // Create the class
      System.out.println("createClassWithMethod");
      Class<?> generatedClass =
          createClassWithMethod(
              ctx,
              new String[] {}, // closure variables  { name }
              new Object[][] {
                // { Integer.class, "new", 5 },     // calling a constructor with "new"
                // { System.out, "println", new Object[]{ Runtime.class, "add", 5, 3 } },
                {"PARSE", "5"},
                {Runtime.class, "print", 789},
                {"PARSE", "@_"}, // retrieve the argument
                {Runtime.class, "print", new Object[] {"PARSE", "5"}},
                {Runtime.class, "print", new Object[] {"PARSE", "@_"}}, // use the argument
                {System.out, "println", "123"},
                // {new Object[] { "PARSE", "5}, "add", 6},
                // { System.out, "println", new Object[]{ new Object[]{ "PARSE", "@_" }, "add", 5
                // }},
                //         // call a method in the argument
                // {new Object[] {"PARSE", "@_"}, "add", 7}, // call a method in the argument
                {"PARSE", "my $a"},
                {"SETVAR", "$a", new Object[] {"PARSE", "12"}},
                {
                  Runtime.class, "print", new Object[] {"PARSE", "$a"},
                },
                {
                  "IF",
                  new Object[] {Runtime.class, "is_false"}, // if condition
                  new Object[][] {{Runtime.class, "print", "if is true"}}, // if block
                  new Object[][] { // else block
                    {Runtime.class, "print", "if is false"},
                    // {"PARSE", "@_"},
                    // {new Object[] {"PARSE", "@_"}, "add", 5}, // call a method in the argument
                    // {
                    //   Runtime.class, "print", new Object[] {"PARSE", "$a"},
                    // },
                    {"PARSE", "my $a"},
                    {"SETVAR", "$a", new Object[] {"PARSE", "13"}},
                    {
                      Runtime.class, "print", new Object[] {"PARSE", "$a"},
                    },
                    {Runtime.class, "print", "end if"},
                  },
                },
                {"PARSE", "$a"},
                {
                  new Object[] {
                    "SUB",
                    new String[] {}, // closure variables  { name }
                    new Object[][] {
                      {System.out, "println", "Inside sub"},
                      {
                        Runtime.class, "print", new Object[] {"PARSE", "$a"}, // closure var
                      },
                      {
                        "IF",
                        new Object[] {Runtime.class, "is_false"}, // if condition
                        new Object[][] {{Runtime.class, "print", "if is true"}}, // if block
                        new Object[][] { // else block
                          {Runtime.class, "print", "if is false"},
                          {"PARSE", "return $a"}, // return from block
                        },
                      },
                      {Runtime.class, "print", new Object[] {"PARSE", "@_"}},
                    }
                  },
                  "apply",
                  new Object[] {"PARSE", "55555"}, ContextType.SCALAR
                },
                {
                  Runtime.class, "print", new Object[] {"PARSE", "$a"},
                },
                {Runtime.class, "print", "end"},
                {"PARSE", "return 5"}
              });

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

  - connect with a Perl parser WIP

  - cleanup the closure code to only add the lexical variables mentioned in the AST

  - format error messages and warnings
        - compile time: get file position from lexer
        - run-time: add annotations to the bytecode

  - test different Perl data types
        - array, hash, string, double, references
        - experiment with Perlito runtime

  - global variables and namespaces
        - named subroutine declaration

  - local variables
      set up the cleanup before RETURN
      set up exception handling

  - add debug information (line numbers)
      Label thisLabel = new Label();
      ctx.mv.visitLabel(thisLabel);
      ctx.mv.visitLineNumber(10, thisLabel); // Associate line number 10 with thisLabel

  - when something is called in void context, we need to POP the JVM stack to cleanup the unused value.
        - "if" in end of sub should inject a "return" in both blocks

  - tests

  - implement thread-safety - it may need locking when calling ASM

  - calling constructor with "new"

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

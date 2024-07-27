import java.lang.reflect.Method;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class ASMMethodCreator implements Opcodes {

  static int classCounter = 0;

  public static Class<?> createClassWithMethod(
      ScopedSymbolTable scope, Object[][] env, Object[][] data) throws Exception {
    // Create a ClassWriter with COMPUTE_FRAMES and COMPUTE_MAXS options
    ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);

    String classNameDot = "org.perlito.anon" + String.valueOf(classCounter++);
    String className = classNameDot.replace('.', '/');

    // Set the source file name for runtime error messages
    cw.visitSource(scope.fileName, null);

    // Define the class with version, access flags, name, signature, superclass, and interfaces
    cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, className, null, "java/lang/Object", null);

    // Add static fields to the class (closed variables)
    for (int i = 0; i < env.length; i++) {
      String fieldName = (String) env[i][0];
      System.out.println("Create static field: " + fieldName);
      cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, fieldName, "LRuntime;", null, null)
          .visitEnd();
    }

    MethodVisitor mv;

    // Create the class initializer method
    mv = cw.visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null);
    mv.visitCode();
    for (int i = 0; i < env.length; i++) { // Initialize the static fields
      String fieldName = (String) env[i][0];
      System.out.println("Init static field: " + fieldName);
      mv.visitTypeInsn(Opcodes.NEW, "Runtime");
      mv.visitInsn(Opcodes.DUP);
      mv.visitMethodInsn(
          Opcodes.INVOKESPECIAL,
          "Runtime",
          "<init>",
          "()V",
          false); // Create a new instance of Runtime
      mv.visitFieldInsn(
          Opcodes.PUTSTATIC, className, fieldName, "LRuntime;"); // Set the static field
    }
    mv.visitInsn(Opcodes.RETURN);
    mv.visitMaxs(0, 0);
    mv.visitEnd();

    // Add a constructor without parameters
    mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
    mv.visitCode();
    mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
    mv.visitMethodInsn(
        Opcodes.INVOKESPECIAL,
        "java/lang/Object",
        "<init>",
        "()V",
        false); // Call the superclass constructor
    mv.visitInsn(Opcodes.RETURN); // Return void
    mv.visitMaxs(0, 0); // Automatically computed
    mv.visitEnd();

    // Create the method
    System.out.println("Create the method");
    String return_type = "(LRuntime;)Ljava/lang/Object;"; // takes an Object arg, returns an Object

    // method is public, static method
    mv =
        cw.visitMethod(
            Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
            "apply",
            return_type,
            null,
            new String[] {"java/lang/Exception"});
    mv.visitCode();

    Label returnLabel = new Label();

    generateCodeBlock(mv, className, scope, data, returnLabel, false); // Process the input data

    System.out.println("Return the last value");
    mv.visitLabel(returnLabel);
    // mv.visitInsn(Opcodes.RETURN);   // returns void
    mv.visitInsn(Opcodes.ARETURN); // returns an Object

    mv.visitMaxs(0, 0); // max stack and local variables
    mv.visitEnd();
    cw.visitEnd(); // complete the class
    byte[] classData = cw.toByteArray(); // generate the bytecode

    CustomClassLoader loader = new CustomClassLoader();
    Class<?> generatedClass = loader.defineClass(classNameDot, classData); // generate the class
    return generatedClass;
  }

  public static void generateCodeBlock(
      MethodVisitor mv,
      String className,
      ScopedSymbolTable scope,
      Object[][] data,
      Label returnLabel,
      boolean isVoidContext)
      throws Exception {
    System.out.println("generateCodeBlock start");
    scope.enterScope();
    for (int i = 0; i < data.length; i++) {
      System.out.println("Process the input data line: " + i);
      processInstructions(
          mv,
          className,
          scope,
          data[i],
          returnLabel,
          isVoidContext
              ? isVoidContext
              : i != (data.length - 1) // void context, except for the last line
          );
    }
    scope.exitScope();
    System.out.println("generateCodeBlock end");
  }

  private static Class<?> processInstructions(
      MethodVisitor mv,
      String className,
      ScopedSymbolTable scope,
      Object[] data,
      Label returnLabel,
      boolean isVoidContext)
      throws Exception {
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
      targetClass =
          processInstructions(mv, className, scope, (Object[]) target, returnLabel, false);
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
        //      mv.visitTypeInsn(Opcodes.NEW, "java/lang/Integer"); // Create a new Integer object
        //      mv.visitInsn(Opcodes.DUP); // Duplicate the top operand stack value
        //      mv.visitVarInsn(Opcodes.ILOAD, 1); // Load the method argument (int value)
        //      mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V",
        // false); // Call the Integer constructor
        throw new Exception("Not implemented: calling a constructor");
      }
    } else if (target instanceof String) {
      System.out.println(" is String");

      if (target.equals("ARG")) { // { "ARG" }
        System.out.println("retrieve arg");
        if (!isVoidContext) {
          System.out.println("  not void context");
          mv.visitVarInsn(Opcodes.ALOAD, 0); // Load argument
        }
        return Runtime.class; // return Class
      } else if (target.equals("GETSTATIC")) { // { "GETSTATIC", "env" }   { GETSTATIC, name }
        System.out.println("retrieve static " + (String) data[1]);
        if (!isVoidContext) {
          mv.visitFieldInsn(Opcodes.GETSTATIC, className, (String) data[1], "LRuntime;");
        }
        return Runtime.class; // return Class
      }
      // } else if (target.equals("GETFIELD")) { // { "GETFIELD", "var" }   { GETFIELD, name }
      //   System.out.println("retrieve field " + (String) data[1]);
      //   mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
      //   mv.visitFieldInsn(Opcodes.GETFIELD, className, (String) data[1], "LRuntime;");
      //   return Runtime.class; // return Class
      // } else if (target.equals("PUTFIELD")) { // { "PUTFIELD", "var" }   { PUTFIELD, name }
      //   System.out.println("put field " + (String) data[1]);
      //   // TODO process argument
      //   // mv.visitVarInsn(Opcodes.ALOAD, 0); // Load 'this'
      //   // mv.visitFieldInsn(Opcodes.PUTFIELD, className, (String) data[1], "LRuntime;");
      //   // return Runtime.class; // return Class
      //   throw new Exception("Not implemented: PUTFIELD");
      else if (target.equals(
          "RETURN")) { // { "RETURN", null, new Object[]{ Runtime.class, "make", 5 } }
        System.out.println(" calling return");
        targetClass = Runtime.class;
        isReturn = true;
      } else if (target.equals("IF")) { // { "IF", cond, if, else }
        System.out.println("IF start");
        scope.enterScope();
        Label elseLabel = new Label();
        Label endLabel = new Label();
        processInstructions(
            mv,
            className,
            scope,
            (Object[]) data[1],
            returnLabel,
            false); // Generate code for the condition
        mv.visitJumpInsn(IFEQ, elseLabel); // Assuming the condition leaves a boolean on the stack
        generateCodeBlock(
            mv,
            className,
            scope,
            (Object[][]) data[2],
            returnLabel,
            true); // Generate code for the if block
        mv.visitJumpInsn(GOTO, endLabel);
        mv.visitLabel(elseLabel);
        if (data[3] != null) { // Generate code for the else block
          generateCodeBlock(mv, className, scope, (Object[][]) data[3], returnLabel, true);
        }
        mv.visitLabel(endLabel); // End of the if/else structure
        scope.exitScope();
        System.out.println("IF end");
        return Runtime.class; // Class of the result
      } else if (target.equals("WHILE")) { // { "WHILE", cond, body }
        System.out.println("WHILE start");
        scope.enterScope();
        Label startLabel = new Label();
        Label endLabel = new Label();

        mv.visitLabel(startLabel);
        processInstructions(
            mv,
            className,
            scope,
            (Object[]) data[1],
            returnLabel,
            false); // Generate code for the condition
        mv.visitJumpInsn(IFEQ, endLabel); // Assuming the condition leaves a boolean on the stack
        generateCodeBlock(
            mv,
            className,
            scope,
            (Object[][]) data[2],
            returnLabel,
            true); // Generate code for the loop body
        mv.visitJumpInsn(GOTO, startLabel); // Jump back to the start of the loop
        mv.visitLabel(endLabel); // End of the loop
        scope.exitScope();
        System.out.println("WHILE end");
        return Runtime.class; // Class of the result
      } else if (target.equals("FOR")) { // { "FOR", init, cond, incr, body }
        System.out.println("FOR start");
        scope.enterScope();
        Label startLabel = new Label();
        Label endLabel = new Label();

        processInstructions(
            mv,
            className,
            scope,
            (Object[]) data[1],
            returnLabel,
            true); // Generate code for the initialization
        mv.visitLabel(startLabel);
        processInstructions(
            mv,
            className,
            scope,
            (Object[]) data[2],
            returnLabel,
            false); // Generate code for the condition
        mv.visitJumpInsn(IFEQ, endLabel); // Assuming the condition leaves a boolean on the stack
        generateCodeBlock(
            mv,
            className,
            scope,
            (Object[][]) data[4],
            returnLabel,
            true); // Generate code for the loop body
        processInstructions(
            mv,
            className,
            scope,
            (Object[]) data[3],
            returnLabel,
            true); // Generate code for the increment
        mv.visitJumpInsn(GOTO, startLabel); // Jump back to the start of the loop
        mv.visitLabel(endLabel); // End of the loop
        scope.exitScope();
        System.out.println("FOR end");
        return Runtime.class; // Class of the result
      } else if (target.equals("MY")) { // { "MY", "$a" }
        System.out.println("MY " + data[1]);
        // TODO set in the scope/frame
        String var = (String) data[1];
        if (scope.getVariableIndexInCurrentScope(var) != -1) {
          System.out.println(
              "Warning: \"my\" variable " + var + " masks earlier declaration in same scope");
        }
        scope.addVariable(var);
        return Runtime.class; // Class of the result
      } else if (target.equals("GETVAR")) { // { "GETVAR", "$a" }
        System.out.println("GETVAR " + data[1]);
        String var = (String) data[1];
        int varIndex = scope.getVariableIndex(var);
        if (varIndex == -1) {
          System.out.println(
              "Warning: Global symbol \""
                  + var
                  + "\" requires explicit package name (did you forget to declare \"my "
                  + var
                  + "\"?)");
        }
        if (!isVoidContext) {
          mv.visitVarInsn(Opcodes.ALOAD, varIndex);
        }
        System.out.println("GETVAR end " + varIndex);
        return Runtime.class; // Class of the result
      } else if (target.equals(
          "SETVAR")) { // { "SETVAR", "$a", new Object[] {Runtime.class, "make", 12} },
        System.out.println("SETVAR " + data[1]);
        String var = (String) data[1];
        int varIndex = scope.getVariableIndex(var);
        if (varIndex == -1) {
          System.out.println(
              "Warning: Global symbol \""
                  + var
                  + "\" requires explicit package name (did you forget to declare \"my "
                  + var
                  + "\"?)");
        }
        if (!isVoidContext) {
          mv.visitInsn(Opcodes.DUP);
        }
        processInstructions(mv, className, scope, (Object[]) data[2], returnLabel, false);
        mv.visitVarInsn(Opcodes.ASTORE, varIndex);
        System.out.println("SETVAR end " + varIndex);
        return Runtime.class; // Class of the result
      } else if (target.equals("SUB")) { // { "SUB", className, env, body }
        System.out.println("SUB start");
        Object[][] newEnv = (Object[][]) data[1]; // env
        Object[][] newData = (Object[][]) data[2]; // data

        Class<?> generatedClass = createClassWithMethod(scope, newEnv, newData);
        generatedClass.getField("env").set(null, new Runtime(111)); // TODO set static field value

        // this will be called at runtime: Runtime.make_sub(className);
        String newClassName = "org.perlito.anon" + String.valueOf(classCounter++);
        Runtime.anonSubs.put(newClassName, generatedClass);
        mv.visitLdcInsn(newClassName);
        mv.visitMethodInsn(
            Opcodes.INVOKESTATIC, "Runtime", "make_sub", "(Ljava/lang/String;)LRuntime;", false);
        System.out.println("SUB end");
        return Runtime.class; // Class of the result
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
    if (data.length > 2) {
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
          Class<?> returnClass =
              processInstructions(mv, className, scope, (Object[]) arg, returnLabel, false);
          argTypes[i] = getPrimitiveClass(returnClass); // process returnClass
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

    if (isReturn) {
      mv.visitJumpInsn(GOTO, returnLabel);
      return targetClass; // Class of the result
    }

    // Fetch the method descriptor
    String methodName = (String) data[1];
    Method method =
        hasArguments
            ? targetClass.getMethod(methodName, argTypes)
            : targetClass.getMethod(methodName);
    String descriptor = org.objectweb.asm.Type.getMethodDescriptor(method);
    System.out.println(
        "call class.method: " + targetClass + " . " + methodName + " descriptor: " + descriptor);

    // Invoke the method
    if (targetIsInstance) {
      System.out.println("invoke virtual");
      mv.visitMethodInsn(
          Opcodes.INVOKEVIRTUAL,
          targetClass.getName().replace('.', '/'),
          methodName,
          descriptor,
          false);
    } else {
      System.out.println("invoke static");
      mv.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          targetClass.getName().replace('.', '/'),
          methodName,
          descriptor,
          false);
    }
    if (isVoidContext && descriptor.charAt(descriptor.length() - 1) != 'V') {
      System.out.println(" in void context");
      mv.visitInsn(Opcodes.POP); // cleanup the stack
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

      // Create the class
      System.out.println("createClassWithMethod");
      Class<?> generatedClass =
          createClassWithMethod(
              new ScopedSymbolTable("test.pl"), // source filename, top-level scope
              new Object[][] { // closed variables  { name }
                {"env"},
              },
              new Object[][] {
                // { Integer.class, "new", 5 },     // calling a constructor with "new"
                // { System.out, "println", new Object[]{ Runtime.class, "add", 5, 3 } },
                {Runtime.class, "make", 5},
                {Runtime.class, "print", 789},
                {"ARG"}, // retrieve the argument
                {Runtime.class, "print", new Object[] {Runtime.class, "make", 5}},
                {Runtime.class, "print", new Object[] {"ARG"}}, // use the argument
                {System.out, "println", "123"},
                {new Object[] {Runtime.class, "make", 5}, "add", 6},
                // { System.out, "println", new Object[]{ new Object[]{ "ARG" }, "add", 5 }},
                //         // call a method in the argument
                {new Object[] {"ARG"}, "add", 7}, // call a method in the argument
                {"MY", "$a"}, // "MY" doesn't generate bytecode
                {"SETVAR", "$a", new Object[] {Runtime.class, "make", 12}},
                {
                  "IF",
                  new Object[] {Runtime.class, "is_false"}, // if condition
                  new Object[][] {{Runtime.class, "print", "if is true"}}, // if block
                  new Object[][] { // else block
                    {Runtime.class, "print", "if is false"},
                    // {"ARG"},
                    // {new Object[] {"ARG"}, "add", 5}, // call a method in the argument
                    // {
                    //   Runtime.class, "print", new Object[] {"GETVAR", "$a"},
                    // },
                    {"MY", "$a"}, // "MY" doesn't generate bytecode
                    {"SETVAR", "$a", new Object[] {Runtime.class, "make", 13}},
                    {
                      Runtime.class, "print", new Object[] {"GETVAR", "$a"},
                    },
                  },
                },
                {
                  Runtime.class, "print", new Object[] {"GETSTATIC", "env"}
                }, // retrieve closed variable
                {
                  new Object[] {
                    "SUB",
                    new Object[][] { // closed variables  { name }
                      {"env"},
                    },
                    new Object[][] {
                      {Runtime.class, "print", new Object[] {"ARG"}},
                    }
                  },
                  "apply",
                  new Object[] {Runtime.class, "make", 55555}
                },
                {
                  Runtime.class, "print", new Object[] {"GETVAR", "$a"},
                },
                {"RETURN", null, new Object[] {Runtime.class, "make", 5}}
              });

      System.out.println("Generated class: " + generatedClass.getName());
      // TODO - move to class definition; create initializer method
      generatedClass.getField("env").set(null, new Runtime(111));

      // Convert into a Runtime object
      String newClassName = "org.perlito.anon" + String.valueOf(classCounter++);
      Runtime.anonSubs.put(newClassName, generatedClass);
      Runtime anonSub = Runtime.make_sub(newClassName);
      Runtime result = anonSub.apply(new Runtime(999));

      System.out.println("Result of generatedMethod: " + result);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}

/* TODO

  - add debug information (line numbers)
      Label thisLabel = new Label();
      mv.visitLabel(thisLabel);
      mv.visitLineNumber(10, thisLabel); // Associate line number 10 with thisLabel

  - when something is called in void context, we need to POP the JVM stack
    to cleanup the unused value. Note: it also works without this
        - "return" in a block should ASTORE the result and then "goto" end of sub,
          instead of push to stack
        - "if" in end of sub should inject a "return" in both blocks

  - tests
    test FOR, WHILE

  - implement thread-safety - it may need locking when calling ASM

  - calling constructor with "new"

  - create multiple classes; ensure GC works for these classes

  - "env" access - create a method to initialize the static field values, instead of using reflection
      generatedClass.getField("env").set(null, mathOps);
      use "clinit" method

  - lexical variables like "my"
      GETFIELD, PUTFIELD
      initialize variables
      create "PAD" information and assign to each block at runtime

  - goto, macros - control structures
  - implement "local"
  - eval, BEGIN-block
  - named subroutine declaration

*/

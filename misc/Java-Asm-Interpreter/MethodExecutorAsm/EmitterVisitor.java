import java.util.List;
import java.util.Map;
import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;

public class EmitterVisitor implements Visitor {
  private final EmitterContext ctx;

  public EmitterVisitor(EmitterContext ctx) {
    this.ctx = ctx;
  }

  @Override
  public void visit(NumberNode node) {
    boolean isInteger = !node.value.contains(".");
    if (ctx.isBoxed) { // expect a Runtime object
      if (isInteger) {
        System.out.println("visit(NumberNode) emit boxed integer");
        ctx.mv.visitTypeInsn(Opcodes.NEW, "Runtime");
        ctx.mv.visitInsn(Opcodes.DUP);
        ctx.mv.visitLdcInsn(
            Integer.valueOf(node.value)); // Push the integer argument onto the stack
        ctx.mv.visitMethodInsn(
            Opcodes.INVOKESPECIAL, "Runtime", "<init>", "(I)V", false); // Call new Runtime(int)
      } else {
        ctx.mv.visitTypeInsn(Opcodes.NEW, "Runtime");
        ctx.mv.visitInsn(Opcodes.DUP);
        ctx.mv.visitLdcInsn(Double.valueOf(node.value)); // Push the double argument onto the stack
        ctx.mv.visitMethodInsn(
            Opcodes.INVOKESPECIAL, "Runtime", "<init>", "(D)V", false); // Call new Runtime(double)
      }
    } else {
      if (isInteger) {
        ctx.mv.visitLdcInsn(Integer.parseInt(node.value)); // emit native integer
      } else {
        ctx.mv.visitLdcInsn(Double.parseDouble(node.value)); // emit native double
      }
    }
    System.out.println("Emit context " + (ctx.contextType == ContextType.VOID ? "void" : "scalar"));
    if (ctx.contextType == ContextType.VOID) {
      ctx.mv.visitInsn(Opcodes.POP);
    }
  }

  @Override
  public void visit(IdentifierNode node) throws Exception {
    // Emit code for identifier
  }

  @Override
  public void visit(BinaryOperatorNode node) throws Exception {
    EmitterVisitor scalarVisitor =
        new EmitterVisitor(ctx.with(ContextType.SCALAR)); // execute operands in scalar context
    node.left.accept(scalarVisitor); // target
    node.right.accept(scalarVisitor); // parameter
    switch (node.operator) {
      case "+":
        node.left.accept(this);
        node.right.accept(this);
        ctx.mv.visitInsn(Opcodes.IADD);
        break;
      case "-":
        ctx.mv.visitInsn(Opcodes.ISUB);
        break;
      case "*":
        ctx.mv.visitInsn(Opcodes.IMUL);
        break;
      case "/":
        ctx.mv.visitInsn(Opcodes.IDIV);
        break;
      case "=":
        ctx.mv.visitMethodInsn(
            Opcodes.INVOKEVIRTUAL,
            "Runtime",
            "set",
            "(LRuntime;)LRuntime;",
            false); // generate a .set() call
        if (ctx.contextType == ContextType.VOID) {
          ctx.mv.visitInsn(Opcodes.POP);
        }
        return;
      case "->":
        if (node.right instanceof ListNode) { // ->()
          System.out.println("visit(BinaryOperatorNode) ->() ");
          ctx.mv.visitFieldInsn(
              Opcodes.GETSTATIC,
              "ContextType",
              ctx.contextType.toString(),
              "LContextType;"); // call context
          ctx.mv.visitMethodInsn(
              Opcodes.INVOKEVIRTUAL,
              "Runtime",
              "apply",
              "(LRuntime;LContextType;)LRuntime;",
              false); // generate an .apply() call
          if (ctx.contextType == ContextType.VOID) {
            ctx.mv.visitInsn(Opcodes.POP);
          }
          return;
        }
        // Add other operators as needed
    }
    throw new RuntimeException("Unexpected infix operator: " + node);
  }

  @Override
  public void visit(UnaryOperatorNode node) throws Exception {
    // Emit code for unary operator
    String operator = node.operator;
    System.out.println("visit(UnaryOperatorNode) " + operator);
    if (operator.equals("$") || operator.equals("@") || operator.equals("%")) {
      String sigil = operator;
      if (node.operand instanceof IdentifierNode) { // $a
        String var = sigil + ((IdentifierNode) node.operand).name;
        System.out.println("GETVAR " + var);
        int varIndex = ctx.symbolTable.getVariableIndex(var);
        if (varIndex == -1) {
          System.out.println(
              "Warning: Global symbol \""
                  + var
                  + "\" requires explicit package name (did you forget to declare \"my "
                  + var
                  + "\"?)");
        }
        if (ctx.contextType != ContextType.VOID) {
          ctx.mv.visitVarInsn(Opcodes.ALOAD, varIndex);
        }
        System.out.println("GETVAR end " + varIndex);
      }
    } else if (operator.equals("print")) {
      EmitterContext childCtx = ctx.with(ContextType.SCALAR, ctx.isBoxed);
      node.operand.accept(new EmitterVisitor(childCtx));
      ctx.mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "Runtime", "print", "()LRuntime;", false);
      if (ctx.contextType == ContextType.VOID) {
        ctx.mv.visitInsn(Opcodes.POP);
      }
    } else if (operator.equals("my")) {
      Node sigilNode = node.operand;
      if (sigilNode instanceof UnaryOperatorNode) { // my + $ @ %
        String sigil = ((UnaryOperatorNode) sigilNode).operator;
        if (sigil.equals("$") || sigil.equals("@") || sigil.equals("%")) {
          Node identifierNode = ((UnaryOperatorNode) sigilNode).operand;
          if (identifierNode instanceof IdentifierNode) { // my $a
            String var = sigil + ((IdentifierNode) identifierNode).name;
            System.out.println("MY " + var);
            if (ctx.symbolTable.getVariableIndexInCurrentScope(var) != -1) {
              System.out.println(
                  "Warning: \"my\" variable "
                      + var
                      + " masks earlier declaration in same ctx.symbolTable");
            }
            int varIndex = ctx.symbolTable.addVariable(var);
            // TODO optimization - SETVAR+MY can be combined
            ctx.mv.visitTypeInsn(Opcodes.NEW, "Runtime");
            ctx.mv.visitInsn(Opcodes.DUP);
            ctx.mv.visitMethodInsn(
                Opcodes.INVOKESPECIAL,
                "Runtime",
                "<init>",
                "()V",
                false); // Create a new instance of Runtime
            if (ctx.contextType != ContextType.VOID) {
              ctx.mv.visitInsn(Opcodes.DUP);
            }
            ctx.mv.visitVarInsn(Opcodes.ASTORE, varIndex);
          }
        }
      }
    } else if (operator.equals("return")) {
      EmitterContext childCtx = ctx.with(ContextType.RUNTIME, ctx.isBoxed);
      node.operand.accept(new EmitterVisitor(childCtx));
      ctx.mv.visitJumpInsn(Opcodes.GOTO, ctx.returnLabel);
    }
  }

  @Override
  public void visit(AnonSubNode node) throws Exception {

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
    Class<?> generatedClass = ASMMethodCreator.createClassWithMethod(subCtx, newEnv, node.block);
    String newClassNameDot = generatedClass.getName();
    String newClassName = newClassNameDot.replace('.', '/');
    System.out.println("Generated class name: " + newClassNameDot + " internal " + newClassName);
    System.out.println("Generated class env:  " + newEnv);

    // initialize the static fields
    // skip 0 and 1 because they are the "@_" argument list and the call context
    for (int i = 2; i < newEnv.length; i++) {
      ctx.mv.visitVarInsn(Opcodes.ALOAD, i); // copy local variable to the new class
      ctx.mv.visitFieldInsn(Opcodes.PUTSTATIC, newClassName, newEnv[i], "LRuntime;");
    }

    // this will be called at runtime: Runtime.make_sub(javaClassName);
    // TODO move the "make_sub" to ASM
    Runtime.anonSubs.put(newClassName, generatedClass);
    ctx.mv.visitLdcInsn(newClassName);
    ctx.mv.visitMethodInsn(
        Opcodes.INVOKESTATIC, "Runtime", "make_sub", "(Ljava/lang/String;)LRuntime;", false);
    System.out.println("SUB end");
  }

  @Override
  public void visit(IfNode node) throws Exception {
    System.out.println("IF start");
    ctx.symbolTable.enterScope();
    Label elseLabel = new Label();
    Label endLabel = new Label();
    EmitterVisitor scalarVisitor =
        new EmitterVisitor(ctx.with(ContextType.SCALAR)); // execute condition in scalar context
    node.condition.accept(scalarVisitor);
    ctx.mv.visitMethodInsn(
        Opcodes.INVOKEVIRTUAL, "Runtime", "toBoolean", "()Z", false); // Call the toBoolean method
    ctx.mv.visitJumpInsn(
        Opcodes.IFEQ, elseLabel); // Assuming the condition leaves a boolean on the stack
    node.thenBranch.accept(scalarVisitor);
    ctx.mv.visitJumpInsn(Opcodes.GOTO, endLabel);
    ctx.mv.visitLabel(elseLabel);
    if (node.elseBranch != null) { // Generate code for the else block
      node.elseBranch.accept(scalarVisitor);
    }
    ctx.mv.visitLabel(endLabel); // End of the if/else structure
    ctx.symbolTable.exitScope();
    System.out.println("IF end");
  }

  @Override
  public void visit(TernaryOperatorNode node) throws Exception {
    node.condition.accept(this);
    // Emit code for ternary operator
  }

  @Override
  public void visit(PostfixOperatorNode node) throws Exception {
    node.operand.accept(this);
    // Emit code for postfix operator
  }

  @Override
  public void visit(BlockNode node) throws Exception {
    System.out.println("generateCodeBlock start");
    ctx.symbolTable.enterScope();
    EmitterVisitor voidVisitor =
        new EmitterVisitor(
            ctx.with(ContextType.VOID)); // statements in the middle of the block have context VOID
    List<Node> list = node.elements;
    for (int i = 0; i < list.size(); i++) {
      Node element = list.get(i);
      if (i == list.size() - 1) {
        // Special case for the last element
        System.out.println("Last element: " + element);
        element.accept(this);
      } else {
        // General case for all other elements
        System.out.println("Element: " + element);
        element.accept(voidVisitor);
      }
    }
    ctx.symbolTable.exitScope();
    System.out.println("generateCodeBlock end");
  }

  @Override
  public void visit(ListNode node) throws Exception {
    for (Node element : node.elements) {
      element.accept(this);
    }
    // Emit code for list
  }

  @Override
  public void visit(StringNode node) throws Exception {
    ctx.mv.visitLdcInsn(node.value); // emit string
  }

  // Add other visit methods as needed
}

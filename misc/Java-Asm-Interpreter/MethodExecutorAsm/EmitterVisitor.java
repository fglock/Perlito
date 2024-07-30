import java.util.List;
import org.objectweb.asm.MethodVisitor;
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
  public void visit(IdentifierNode node) {
    // Emit code for identifier
  }

  @Override
  public void visit(BinaryOperatorNode node) {
    node.left.accept(this);
    node.right.accept(this);
    switch (node.operator) {
      case "+":
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
        // Add other operators as needed
    }
  }

  @Override
  public void visit(UnaryOperatorNode node) {
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
    }
    else if (operator.equals("return")) {
      EmitterContext childCtx = ctx.with(ContextType.RUNTIME, ctx.isBoxed);
      node.operand.accept(new EmitterVisitor(childCtx));
      ctx.mv.visitJumpInsn(Opcodes.GOTO, ctx.returnLabel);
    }
  }

  @Override
  public void visit(IfNode node) {
    System.out.println("IF start");
    ctx.symbolTable.enterScope();
    Label elseLabel = new Label();
    Label endLabel = new Label();
    EmitterVisitor scalarVisitor =
        new EmitterVisitor(
        ctx.with(ContextType.SCALAR)); // execute condition in scalar context
    node.condition.accept(scalarVisitor);
    ctx.mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "Runtime", "toBoolean", "()Z", false);  // Call the toBoolean method
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
  public void visit(TernaryOperatorNode node) {
    node.condition.accept(this);
    // Emit code for ternary operator
  }

  @Override
  public void visit(PostfixOperatorNode node) {
    node.operand.accept(this);
    // Emit code for postfix operator
  }

  @Override
  public void visit(BlockNode node) {
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
  public void visit(ListNode node) {
    for (Node element : node.elements) {
      element.accept(this);
    }
    // Emit code for list
  }

  @Override
  public void visit(StringNode node) {
    ctx.mv.visitLdcInsn(node.value); // emit string
  }

  // Add other visit methods as needed
}

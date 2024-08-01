import org.objectweb.asm.Label;
import org.objectweb.asm.Opcodes;
import java.util.*;

public class EmitterVisitor implements Visitor {
  private final EmitterContext ctx;

  /** Cache for EmitterVisitor instances with different ContextTypes */
  private final Map<ContextType, EmitterVisitor> visitorCache = new EnumMap<>(ContextType.class);

  public EmitterVisitor(EmitterContext ctx) {
    this.ctx = ctx;
  }

  /**
   * Returns an EmitterVisitor with the specified context type.
   * Uses a cache to avoid creating new instances unnecessarily.
   *
   * <p>Example usage:</p>
   * <pre>
   *   // emits the condition code in scalar context
   *   node.condition.accept(this.with(ContextType.SCALAR));
   * </pre>
   *
   * @param contextType The context type for the new EmitterVisitor.
   * @return An EmitterVisitor with the specified context type.
   */
  public EmitterVisitor with(ContextType contextType) {
    // Check if the visitor is already cached
    if (visitorCache.containsKey(contextType)) {
      return visitorCache.get(contextType);
    }
    // Create a new visitor and cache it
    EmitterVisitor newVisitor = new EmitterVisitor(ctx.with(contextType));
    visitorCache.put(contextType, newVisitor);
    return newVisitor;
  }

  @Override
  public void visit(NumberNode node) {
    ctx.logDebug("visit(NumberNode) in context " + ctx.contextType);
    if (ctx.contextType == ContextType.VOID) {
      return;
    }
    boolean isInteger = !node.value.contains(".");
    if (ctx.isBoxed) { // expect a Runtime object
      if (isInteger) {
        ctx.logDebug("visit(NumberNode) emit boxed integer");
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
  }

  @Override
  public void visit(IdentifierNode node) throws Exception {
    // Emit code for identifier
    throw new PerlCompilerException(node.tokenIndex, "Not implemented: bare word " + node.name, ctx.errorUtil);
  }

  /**
   * Emits a call to a built-in method on the Runtime class.
   * It assumes that the parameter to the call is already in the stack.
   * 
   * @param operator The name of the built-in method to call.
   */
  private void handleBinaryBuiltin(String operator) {
    ctx.mv.visitMethodInsn(
         Opcodes.INVOKEVIRTUAL,
         "Runtime",
         operator,
         "(LRuntime;)LRuntime;",
         false);
     if (ctx.contextType == ContextType.VOID) {
       ctx.mv.visitInsn(Opcodes.POP);
     }
  }

@Override
  public void visit(BinaryOperatorNode node) throws Exception {
    String operator = node.operator;
    ctx.logDebug("visit(BinaryOperatorNode) " + operator + " in context " + ctx.contextType);
    EmitterVisitor scalarVisitor = this.with(ContextType.SCALAR); // execute operands in scalar context
    node.left.accept(scalarVisitor); // target
    node.right.accept(scalarVisitor); // parameter

    switch (operator) {
        case "+":
            handleBinaryBuiltin("add"); // TODO optimize use: ctx.mv.visitInsn(Opcodes.IADD)
            break;
        case "-":
            handleBinaryBuiltin("subtract");
            break;
        case "*":
            handleBinaryBuiltin("multiply");
            break;
        case "/":
            handleBinaryBuiltin("divide");
            break;
        case ".":
            handleBinaryBuiltin("stringConcat");
            break;
        case "=":
            handleBinaryBuiltin("set");
            break;
        case "->":
            handleArrowOperator(node);
            break;
        default:
            throw new RuntimeException("Unexpected infix operator: " + operator);
    }
  }

  /**
   * Handles the `->` operator.
   */
  private void handleArrowOperator(BinaryOperatorNode node) throws Exception {
    if (node.right instanceof ListNode) { // ->()
        ctx.logDebug("visit(BinaryOperatorNode) ->() ");
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
    } else {
        throw new RuntimeException("Unexpected right operand for `->` operator: " + node.right);
    }
  }

  @Override
  public void visit(UnaryOperatorNode node) throws Exception {
    String operator = node.operator;
    ctx.logDebug("visit(UnaryOperatorNode) " + operator + " in context " + ctx.contextType);

    switch (operator) {
        case "$":
        case "@":
        case "%":
            handleVariableOperator(node, operator);
            break;
        case "print":
            handlePrintOperator(node);
            break;
        case "my":
            handleMyOperator(node);
            break;
        case "return":
            handleReturnOperator(node);
            break;
        case "eval":
            handleEvalOperator(node);
            break;
        default:
            throw new UnsupportedOperationException("Unsupported operator: " + operator);
    }
  }

  private void handleVariableOperator(UnaryOperatorNode node, String operator) throws Exception {
    String sigil = operator;
    if (node.operand instanceof IdentifierNode) { // $a @a %a
        String var = sigil + ((IdentifierNode) node.operand).name;
        ctx.logDebug("GETVAR " + var);
        int varIndex = ctx.symbolTable.getVariableIndex(var);
        if (varIndex == -1) {
            ctx.logDebug(
                "Warning: Global symbol \""
                + var
                + "\" requires explicit package name (did you forget to declare \"my "
                + var
                + "\"?)"
            );
        }
        if (ctx.contextType != ContextType.VOID) {
            ctx.mv.visitVarInsn(Opcodes.ALOAD, varIndex);
        }
        ctx.logDebug("GETVAR end " + varIndex);
        return;
    }
    // TODO special variables $1 $`
    // TODO ${a} ${[ 123 ]}
    throw new PerlCompilerException(node.tokenIndex, "Not implemented: " + operator, ctx.errorUtil);
  }

  private void handlePrintOperator(UnaryOperatorNode node) throws Exception {
    node.operand.accept(this.with(ContextType.SCALAR));
    ctx.mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "Runtime", "print", "()LRuntime;", false);
    if (ctx.contextType == ContextType.VOID) {
        ctx.mv.visitInsn(Opcodes.POP);
    }
    // TODO print FILE 123
  }

  private void handleMyOperator(UnaryOperatorNode node) throws Exception {
    Node sigilNode = node.operand;
    if (sigilNode instanceof UnaryOperatorNode) { // my + $ @ %
        String sigil = ((UnaryOperatorNode) sigilNode).operator;
        if (sigil.equals("$") || sigil.equals("@") || sigil.equals("%")) {
            Node identifierNode = ((UnaryOperatorNode) sigilNode).operand;
            if (identifierNode instanceof IdentifierNode) { // my $a
                String var = sigil + ((IdentifierNode) identifierNode).name;
                ctx.logDebug("MY " + var);
                if (ctx.symbolTable.getVariableIndexInCurrentScope(var) != -1) {
                    ctx.logDebug(
                        "Warning: \"my\" variable "
                        + var
                        + " masks earlier declaration in same ctx.symbolTable"
                    );
                }
                int varIndex = ctx.symbolTable.addVariable(var);
                // TODO optimization - SETVAR+MY can be combined
                ctx.mv.visitTypeInsn(Opcodes.NEW, "Runtime");
                ctx.mv.visitInsn(Opcodes.DUP);
                ctx.mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "Runtime", "<init>", "()V", false); // Create a new instance of Runtime
                if (ctx.contextType != ContextType.VOID) {
                    ctx.mv.visitInsn(Opcodes.DUP);
                }
                ctx.mv.visitVarInsn(Opcodes.ASTORE, varIndex);
                return;
            }
        }
    }
    // TODO my ($a, $b)
    throw new PerlCompilerException(node.tokenIndex, "Not implemented: " + node.operator, ctx.errorUtil);
  }

  private void handleReturnOperator(UnaryOperatorNode node) throws Exception {
    node.operand.accept(this.with(ContextType.RUNTIME));
    ctx.mv.visitJumpInsn(Opcodes.GOTO, ctx.returnLabel);
    // TODO return (1,2), 3
  }

  private void handleEvalOperator(UnaryOperatorNode node) throws Exception {
    if (node.operand instanceof BlockNode) { // eval block
        // TODO eval block
        throw new PerlCompilerException(node.tokenIndex, "Not implemented: eval block", ctx.errorUtil);
    } else { // eval string
            // evaluate the string in scalar context
            // push the code string to the stack
            node.operand.accept(this.with(ContextType.SCALAR));

            // save the compiler context in Runtime class
            // push a reference to the compiler content to the stack
            String evalTag = "eval" + Integer.toString(ASMMethodCreator.classCounter++);
            // create the eval context
            EmitterContext evalCtx =
                new EmitterContext(
                    "(eval)", // filename
                    ASMMethodCreator.generateClassName(), // internal java class name
                    ctx.symbolTable.clone(), // clone the symbolTable
                    null, // return label
                    null, // method visitor
                    ctx.contextType, // call context
                    true, // is boxed
                    ctx.errorUtil,  // error message utility
                    ctx.debugEnabled
                    );
            Runtime.evalContext.put(evalTag, evalCtx);  // XXX TODO save a deep copy
            ctx.mv.visitLdcInsn(evalTag);   // push the evalTag to the stack

            // call Runtime.eval_string()
            ctx.mv.visitMethodInsn(
                Opcodes.INVOKESTATIC, "Runtime", "eval_string", "(LRuntime;Ljava/lang/String;)LRuntime;", false);


            // TODO at compile time ----------------
            //
            //  // initialize the static fields
            //  // skip 0 and 1 because they are the "@_" argument list and the call context
            //  for (int i = 2; i < newEnv.length; i++) {
            //    ctx.mv.visitVarInsn(Opcodes.ALOAD, i); // copy local variable to the new class
            //    ctx.mv.visitFieldInsn(Opcodes.PUTSTATIC, newClassName, newEnv[i], "LRuntime;");
            //  }


            //  // Convert the generated class into a Runtime object
            //  String newClassName = generatedClass.getName();
            //  Runtime.anonSubs.put(newClassName, generatedClass); // Store the class in the runtime map
            //  Runtime anonSub = Runtime.make_sub(newClassName); // Create a Runtime instance for the generated class
            //  Runtime result = anonSub.apply(new Runtime(), ContextType.SCALAR); // Execute the generated method


            if (ctx.contextType == ContextType.VOID) {
              ctx.mv.visitInsn(Opcodes.POP);
            }
            return;
    }
  }

  @Override
  public void visit(AnonSubNode node) throws Exception {

    ctx.logDebug("SUB start");

    // retrieve closure variable list
    // alternately, scan the AST for variables and capture only the ones that are used
    Map<Integer, String> visibleVariables = ctx.symbolTable.getAllVisibleVariables();
    String[] newEnv = new String[visibleVariables.size()];
    ctx.logDebug(" ctx.symbolTable.getAllVisibleVariables");
    for (Integer index : visibleVariables.keySet()) {
      String variableName = visibleVariables.get(index);
      ctx.logDebug("  " + index + " " + variableName);
      newEnv[index] = variableName;
    }

    // create the new method
    EmitterContext subCtx =
        new EmitterContext(
            ctx.fileName, // same source filename
            ASMMethodCreator.generateClassName(), // internal java class name
            ctx.symbolTable, // closure symbolTable
            null, // return label
            null, // method visitor
            ContextType.RUNTIME, // call context
            true, // is boxed
            ctx.errorUtil,  // error message utility
            ctx.debugEnabled
            );
    Class<?> generatedClass = ASMMethodCreator.createClassWithMethod(subCtx, newEnv, node.block);
    String newClassNameDot = generatedClass.getName();
    String newClassName = newClassNameDot.replace('.', '/');
    ctx.logDebug("Generated class name: " + newClassNameDot + " internal " + newClassName);
    ctx.logDebug("Generated class env:  " + newEnv);

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
    ctx.logDebug("SUB end");
  }

  @Override
  public void visit(IfNode node) throws Exception {
    ctx.logDebug("IF start");
    
    // Enter a new scope in the symbol table
    ctx.symbolTable.enterScope();
    
    // Create labels for the else and end branches
    Label elseLabel = new Label();
    Label endLabel = new Label();
    
    // Visit the condition node in scalar context
    node.condition.accept(this.with(ContextType.SCALAR));
    
    // Convert the result to a boolean
    ctx.mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "Runtime", "toBoolean", "()Z", false);
    
    // Jump to the else label if the condition is false
    ctx.mv.visitJumpInsn(Opcodes.IFEQ, elseLabel);
    
    // Visit the then branch
    node.thenBranch.accept(this);
    
    // Jump to the end label after executing the then branch
    ctx.mv.visitJumpInsn(Opcodes.GOTO, endLabel);
    
    // Visit the else label
    ctx.mv.visitLabel(elseLabel);
    
    // Visit the else branch if it exists
    if (node.elseBranch != null) {
        node.elseBranch.accept(this);
    }
    
    // Visit the end label
    ctx.mv.visitLabel(endLabel);
    
    // Exit the scope in the symbol table
    ctx.symbolTable.exitScope();
    
    ctx.logDebug("IF end");
  }


  @Override
  public void visit(TernaryOperatorNode node) throws Exception {
    node.condition.accept(this);
    // Emit code for ternary operator
    throw new PerlCompilerException(node.tokenIndex, "Not implemented: ternary operator", ctx.errorUtil);
  }

  @Override
  public void visit(PostfixOperatorNode node) throws Exception {
    node.operand.accept(this);
    // Emit code for postfix operator
    throw new PerlCompilerException(node.tokenIndex, "Not implemented: postfix operator " + node.operator, ctx.errorUtil);
  }

  @Override
  public void visit(BlockNode node) throws Exception {
    ctx.logDebug("generateCodeBlock start");
    ctx.symbolTable.enterScope();
    EmitterVisitor voidVisitor = this.with(ContextType.VOID);  // statements in the middle of the block have context VOID
    List<Node> list = node.elements;
    for (int i = 0; i < list.size(); i++) {
      Node element = list.get(i);
      if (i == list.size() - 1) {
        // Special case for the last element
        ctx.logDebug("Last element: " + element);
        element.accept(this);
      } else {
        // General case for all other elements
        ctx.logDebug("Element: " + element);
        element.accept(voidVisitor);
      }
    }
    ctx.symbolTable.exitScope();
    ctx.logDebug("generateCodeBlock end");
  }

  @Override
  public void visit(ListNode node) throws Exception {
    // Emit code for list
    for (Node element : node.elements) {
      element.accept(this);
    }
  }

  @Override
  public void visit(StringNode node) throws Exception {
    if (ctx.contextType == ContextType.VOID) {
      return;
    }
    if (ctx.isBoxed) { // expect a Runtime object
        ctx.mv.visitTypeInsn(Opcodes.NEW, "Runtime");
        ctx.mv.visitInsn(Opcodes.DUP);
        ctx.mv.visitLdcInsn(node.value); // emit string
        ctx.mv.visitMethodInsn(
            Opcodes.INVOKESPECIAL, "Runtime", "<init>", "(Ljava/lang/String;)V", false); // Call new Runtime(String)
    } else {
      ctx.mv.visitLdcInsn(node.value); // emit string
    }
  }

  // Add other visit methods as needed
}

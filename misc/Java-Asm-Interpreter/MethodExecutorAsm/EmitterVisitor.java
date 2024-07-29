import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class EmitterVisitor implements Visitor {
    private final MethodVisitor mv;

    public EmitterVisitor(MethodVisitor mv) {
        this.mv = mv;
    }

    @Override
    public void visit(NumberNode node) {
        if (node.value.contains(".")) {
            mv.visitLdcInsn(Double.valueOf(node.value)); // emit double
        } else {
            mv.visitLdcInsn(Integer.valueOf(node.value)); // emit integer
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
                mv.visitInsn(Opcodes.IADD);
                break;
            case "-":
                mv.visitInsn(Opcodes.ISUB);
                break;
            case "*":
                mv.visitInsn(Opcodes.IMUL);
                break;
            case "/":
                mv.visitInsn(Opcodes.IDIV);
                break;
            // Add other operators as needed
        }
    }

    @Override
    public void visit(UnaryOperatorNode node) {
        node.operand.accept(this);
        // Emit code for unary operator
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
    public void visit(ListNode node) {
        for (Node element : node.elements) {
            element.accept(this);
        }
        // Emit code for list
    }

    @Override
    public void visit(StringNode node) {
        mv.visitLdcInsn(node.value); // emit string
    }

    // Add other visit methods as needed
}

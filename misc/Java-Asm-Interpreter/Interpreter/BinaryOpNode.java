import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class BinaryOpNode extends CodeGeneratingNode {
    private final char operator;
    private final Node left;
    private final Node right;

    public BinaryOpNode(char operator, Node left, Node right) {
        this.operator = operator;
        this.left = left;
        this.right = right;
    }

    @Override
    public int evaluate() {
        int leftValue = left.evaluate();
        int rightValue = right.evaluate();
        switch (operator) {
            case '+': return leftValue + rightValue;
            case '-': return leftValue - rightValue;
            case '*': return leftValue * rightValue;
            case '/': return leftValue / rightValue;
            case '%': return leftValue % rightValue;
            default: throw new RuntimeException("Unknown operator: " + operator);
        }
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        left.generateCode(mv);
        right.generateCode(mv);
        switch (operator) {
            case '+': mv.visitInsn(Opcodes.IADD); break;
            case '-': mv.visitInsn(Opcodes.ISUB); break;
            case '*': mv.visitInsn(Opcodes.IMUL); break;
            case '/': mv.visitInsn(Opcodes.IDIV); break;
            case '%': mv.visitInsn(Opcodes.IREM); break;
            default: throw new RuntimeException("Unknown operator: " + operator);
        }
    }
}


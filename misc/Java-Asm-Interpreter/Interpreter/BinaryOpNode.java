import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class BinaryOpNode extends CodeGeneratingNode {
    private final TokenType op;
    private final Node left;
    private final Node right;

    public BinaryOpNode(TokenType op, Node left, Node right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }

    @Override
    public int evaluate() {
        // Stub for evaluation logic
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        left.generateCode(mv);
        right.generateCode(mv);
        switch (op) {
            case PLUS -> mv.visitInsn(Opcodes.IADD);
            case MINUS -> mv.visitInsn(Opcodes.ISUB);
            case STAR -> mv.visitInsn(Opcodes.IMUL);
            case SLASH -> mv.visitInsn(Opcodes.IDIV);
            default -> throw new UnsupportedOperationException("Unsupported operation: " + op);
        }
    }
}

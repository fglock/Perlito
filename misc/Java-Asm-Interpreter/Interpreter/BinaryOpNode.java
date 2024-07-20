import org.objectweb.asm.MethodVisitor;
import static org.objectweb.asm.Opcodes.*;

public class BinaryOpNode extends Node {
    private Node left;
    private Node right;
    private String op;

    public BinaryOpNode(Node left, Node right, String op) {
        this.left = left;
        this.right = right;
        this.op = op;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        left.generateCode(mv);
        right.generateCode(mv);
        switch (op) {
            case "+":
                mv.visitInsn(IADD);
                break;
            case "-":
                mv.visitInsn(ISUB);
                break;
            case "*":
                mv.visitInsn(IMUL);
                break;
            case "/":
                mv.visitInsn(IDIV);
                break;
            case "**":
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Math", "pow", "(DD)D", false);
                mv.visitInsn(D2I); // convert double result back to int
                break;
        }
    }
}


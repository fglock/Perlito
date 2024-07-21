import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class PrintNode extends CodeGeneratingNode {
    private final Node expression;

    public PrintNode(Node expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate() {
        // Stub for evaluation logic
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        expression.generateCode(mv);
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/io/PrintStream", "println", "(I)V", false);
    }
}

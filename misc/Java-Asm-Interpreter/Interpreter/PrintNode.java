import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class PrintNode extends CodeGeneratingNode {
    private final Node expression;

    public PrintNode(Node expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate() {
        int value = expression.evaluate();
        System.out.println(value);
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        expression.generateCode(mv);
        mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        mv.visitInsn(Opcodes.SWAP);
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false);
    }
}

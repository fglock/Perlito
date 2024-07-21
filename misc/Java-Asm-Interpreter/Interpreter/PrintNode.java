import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class PrintNode extends CodeGeneratingNode {
    private final Node expression;

    public PrintNode(Node expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate() {
        System.out.println(expression.evaluate());
        return 1;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        expression.generateCode(mv);
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false);
    }
}


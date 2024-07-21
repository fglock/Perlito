import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class ReturnNode extends CodeGeneratingNode {
    private final Node expression;

    public ReturnNode(Node expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate() {
        return expression.evaluate();
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        expression.generateCode(mv);
        mv.visitInsn(Opcodes.IRETURN);
    }
}

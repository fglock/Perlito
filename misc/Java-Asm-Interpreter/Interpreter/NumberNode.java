import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class NumberNode extends CodeGeneratingNode {
    private final double value;

    public NumberNode(double value) {
        this.value = value;
    }

    @Override
    public int evaluate() {
        return (int) value;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        mv.visitLdcInsn((int) value);
    }
}

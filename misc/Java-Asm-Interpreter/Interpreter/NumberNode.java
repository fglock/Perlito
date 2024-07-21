import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class NumberNode extends CodeGeneratingNode {
    private final int value;

    public NumberNode(int value) {
        this.value = value;
    }

    @Override
    public int evaluate() {
        return value;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        mv.visitLdcInsn(value);
    }
}


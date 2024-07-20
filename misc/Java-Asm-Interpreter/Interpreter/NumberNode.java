import org.objectweb.asm.MethodVisitor;
import static org.objectweb.asm.Opcodes.*;

public class NumberNode extends Node {
    private int value;

    public NumberNode(int value) {
        this.value = value;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        mv.visitLdcInsn(value);
    }
}


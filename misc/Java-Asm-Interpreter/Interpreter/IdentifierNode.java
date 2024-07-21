import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class IdentifierNode extends CodeGeneratingNode {
    private final String name;
    private final int index;

    public IdentifierNode(String name, int index) {
        this.name = name;
        this.index = index;
    }

    public String getName() {
        return name;
    }

    public int getIndex() {
        return index;
    }

    @Override
    public int evaluate() {
        // Stub for evaluation logic
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        if (index < 0) {
            throw new UnsupportedOperationException("IdentifierNode cannot generate code without a valid index");
        }
        mv.visitVarInsn(Opcodes.ILOAD, index);
    }
}

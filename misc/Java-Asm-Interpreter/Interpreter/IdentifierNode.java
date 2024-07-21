import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class IdentifierNode extends CodeGeneratingNode {
    private final String name;
    private final int localIndex;

    public IdentifierNode(String name, int localIndex) {
        this.name = name;
        this.localIndex = localIndex;
    }

    public String getName() {
        return name;
    }

    @Override
    public int evaluate() {
        throw new UnsupportedOperationException("IdentifierNode cannot be evaluated directly.");
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        mv.visitVarInsn(Opcodes.ILOAD, localIndex);
    }
}


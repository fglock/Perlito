import org.objectweb.asm.MethodVisitor;

public class IdentifierNode extends Node {
    private final String name;

    public IdentifierNode(String name) {
        this.name = name;
    }

    @Override
    public int evaluate() {
        throw new UnsupportedOperationException("IdentifierNode cannot be evaluated directly.");
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        throw new UnsupportedOperationException("IdentifierNode cannot generate code directly.");
    }

    public String getName() {
        return name;
    }
}


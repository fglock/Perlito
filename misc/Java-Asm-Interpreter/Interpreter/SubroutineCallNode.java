import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SubroutineCallNode extends Node {
    private final String name;
    private final Node argument;

    public SubroutineCallNode(String name, Node argument) {
        this.name = name;
        this.argument = argument;
    }

    @Override
    public int evaluate() {
        throw new UnsupportedOperationException("SubroutineCallNode cannot be evaluated directly.");
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        argument.generateCode(mv);
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "CompiledExpression", name, "(I)I", false);
    }
}


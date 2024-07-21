import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SubroutineCallNode extends CodeGeneratingNode {
    private final String name;
    private final Node argument;

    public SubroutineCallNode(String name, Node argument) {
        this.name = name;
        this.argument = argument;
    }

    @Override
    public int evaluate() {
        // This will not be used in this context
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        // Generate code for the argument
        argument.generateCode(mv);

        // Call the subroutine
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "CompiledExpression", name, "(I)I", false);
    }
}


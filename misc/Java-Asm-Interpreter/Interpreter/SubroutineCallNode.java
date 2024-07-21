import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.util.List;

public class SubroutineCallNode extends CodeGeneratingNode {
    private final String name;
    private final List<Node> arguments;

    public SubroutineCallNode(String name, List<Node> arguments) {
        this.name = name;
        this.arguments = arguments;
    }

    @Override
    public int evaluate() {
        // Stub for evaluation logic
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        for (Node argument : arguments) {
            argument.generateCode(mv);
        }
        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "CompiledExpression", name, getDescriptor(), false);
    }

    private String getDescriptor() {
        StringBuilder sb = new StringBuilder();
        sb.append('(');
        for (Node argument : arguments) {
            sb.append('I');
        }
        sb.append(")I");
        return sb.toString();
    }
}

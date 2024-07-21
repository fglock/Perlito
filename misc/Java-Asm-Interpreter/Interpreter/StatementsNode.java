import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.util.List;

public class StatementsNode extends CodeGeneratingNode {
    private final List<Node> statements;

    public StatementsNode(List<Node> statements) {
        this.statements = statements;
    }

    @Override
    public int evaluate() {
        // Stub for evaluation logic
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        for (Node statement : statements) {
            statement.generateCode(mv);
        }
    }
}

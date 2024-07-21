import org.objectweb.asm.MethodVisitor;

import java.util.List;

public class StatementsNode extends CodeGeneratingNode {
    private final List<Node> statements;

    public StatementsNode(List<Node> statements) {
        this.statements = statements;
    }

    @Override
    public int evaluate() {
        int result = 0;
        for (Node statement : statements) {
            result = statement.evaluate();
        }
        return result;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        for (Node statement : statements) {
            statement.generateCode(mv);
        }
    }
}

import org.objectweb.asm.MethodVisitor;

import java.util.List;

public class BlockNode extends Node {
    private final List<Node> statements;

    public BlockNode(List<Node> statements) {
        this.statements = statements;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        for (Node statement : statements) {
            statement.generateCode(mv);
        }
    }
}

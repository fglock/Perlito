import org.objectweb.asm.MethodVisitor;
import static org.objectweb.asm.Opcodes.*;

import java.util.List;

public class StatementsNode extends Node {
    private List<Node> statements;

    public StatementsNode(List<Node> statements) {
        this.statements = statements;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        for (Node stmt : statements) {
            stmt.generateCode(mv);
        }
    }
}


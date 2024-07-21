import org.objectweb.asm.MethodVisitor;

import java.util.List;

public class BlockNode extends CodeGeneratingNode {
    private final List<CodeGeneratingNode> statements;

    public BlockNode(List<CodeGeneratingNode> statements) {
        this.statements = statements;
    }

    @Override
    public int evaluate() {
        int result = 0;
        for (CodeGeneratingNode statement : statements) {
            result = statement.evaluate();
        }
        return result;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        for (CodeGeneratingNode statement : statements) {
            statement.generateCode(mv);
        }
    }
}


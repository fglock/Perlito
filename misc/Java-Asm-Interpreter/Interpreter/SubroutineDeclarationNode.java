import org.objectweb.asm.MethodVisitor;
import static org.objectweb.asm.Opcodes.*;

public class SubroutineDeclarationNode extends Node {
    private String name;
    private Node body;

    public SubroutineDeclarationNode(String name, Node body) {
        this.name = name;
        this.body = body;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        // Assuming subroutine body is executed directly for simplicity
        body.generateCode(mv);
    }
}


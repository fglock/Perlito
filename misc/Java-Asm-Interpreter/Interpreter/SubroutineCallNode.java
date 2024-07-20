import org.objectweb.asm.MethodVisitor;

import java.util.List;

public class SubroutineCallNode extends Node {
    private final String subroutineName;
    private final List<Node> arguments;

    public SubroutineCallNode(String subroutineName, List<Node> arguments) {
        this.subroutineName = subroutineName;
        this.arguments = arguments;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        // Handle subroutine call
    }
}

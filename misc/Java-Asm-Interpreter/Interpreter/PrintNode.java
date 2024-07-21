import org.objectweb.asm.MethodVisitor;
import static org.objectweb.asm.Opcodes.*;

public class PrintNode extends Node {
    private Node expression;

    public PrintNode(Node expression) {
        this.expression = expression;
    }


    @Override
    public int evaluate() {
        System.out.println(expression.evaluate());
        return 1;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        // Generate code to evaluate the expression
        expression.generateCode(mv);

        // Print the evaluated value
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        mv.visitInsn(SWAP);
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false);
    }
}


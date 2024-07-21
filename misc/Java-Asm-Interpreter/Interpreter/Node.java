import org.objectweb.asm.MethodVisitor;

public abstract class Node {
    public abstract int evaluate();

    public abstract void generateCode(MethodVisitor mv);
}


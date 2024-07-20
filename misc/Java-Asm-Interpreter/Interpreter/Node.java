import org.objectweb.asm.MethodVisitor;

public abstract class Node {
    public abstract void generateCode(MethodVisitor mv);
}

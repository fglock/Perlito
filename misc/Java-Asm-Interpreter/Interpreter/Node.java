import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;

public abstract class Node {
    public abstract int evaluate();
    public abstract void generateCode(MethodVisitor mv);
    public abstract void generateCode(ClassWriter cw);
}

abstract class CodeGeneratingNode extends Node {
    @Override
    public abstract void generateCode(MethodVisitor mv);

    // Remove the @Override annotation here as this method does not override any method in Node
    public void generateCode(ClassWriter cw) {
        throw new UnsupportedOperationException("CodeGeneratingNode cannot generateCode(ClassWriter).");
    }
}

abstract class MethodDefiningNode extends Node {
    @Override
    public abstract void generateCode(ClassWriter cw);

    // Remove the @Override annotation here as this method does not override any method in Node
    public void generateCode(MethodVisitor mv) {
        throw new UnsupportedOperationException("MethodDefiningNode cannot generateCode(MethodVisitor).");
    }
}

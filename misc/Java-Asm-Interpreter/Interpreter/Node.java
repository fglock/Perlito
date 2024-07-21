import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;

public abstract class Node {
    public abstract int evaluate();
    public abstract void generateCode(MethodVisitor mv);
    public abstract void generateCode(ClassWriter cw);
}

abstract class CodeGeneratingNode extends Node {
    public abstract void generateCode(MethodVisitor mv);

    @Override
    public void generateCode(ClassWriter cw) {
        throw new UnsupportedOperationException("CodeGeneratingNode cannot generateCode(ClassWriter).");
    }
}

abstract class MethodDefiningNode extends Node {
    public abstract void generateCode(ClassWriter cw);

    @Override
    public void generateCode(MethodVisitor mv) {
        throw new UnsupportedOperationException("MethodDefiningNode cannot generateCode(MethodVisitor).");
    }
}

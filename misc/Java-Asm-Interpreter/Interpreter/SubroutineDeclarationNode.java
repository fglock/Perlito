import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.util.List;

public class SubroutineDeclarationNode extends Node {
    private final String name;
    private final List<String> parameters;
    private final Node body;

    public SubroutineDeclarationNode(String name, List<String> parameters, Node body) {
        this.name = name;
        this.parameters = parameters;
        this.body = body;
    }

    @Override
    public int evaluate() {
        throw new UnsupportedOperationException("SubroutineDeclarationNode cannot be evaluated directly.");
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        throw new UnsupportedOperationException("SubroutineDeclarationNode cannot generate code directly.");
    }

    public void generateCode(ClassWriter cw) {
        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, name, "(I)I", null, null);
        mv.visitCode();
        body.generateCode(mv);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
    }

    public String getName() {
        return name;
    }
}


import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.util.List;

public class SubroutineDeclarationNode extends MethodDefiningNode {
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
        // Placeholder: Actual implementation would involve managing a symbol table and execution context
        return 0;
    }

    @Override
    public void generateCode(MethodVisitor mv) {
        throw new UnsupportedOperationException("SubroutineDeclarationNode cannot generate code with MethodVisitor");
    }

    @Override
    public void generateCode(ClassWriter cw) {
        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, name, getMethodDescriptor(), null, null);
        mv.visitCode();
        body.generateCode(mv);
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }

    private String getMethodDescriptor() {
        StringBuilder descriptor = new StringBuilder("(");
        for (String param : parameters) {
            descriptor.append("I");
        }
        descriptor.append(")I");
        return descriptor.toString();
    }
}

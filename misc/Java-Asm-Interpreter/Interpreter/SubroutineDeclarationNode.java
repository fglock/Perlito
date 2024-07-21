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
        // Stub for evaluation logic
        return 0;
    }

    @Override
    public void generateCode(ClassWriter cw) {
        String methodDescriptor = parameters.isEmpty() ? "()I" : "(" + "I".repeat(parameters.size()) + ")I";
        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, name, methodDescriptor, null, null);
        mv.visitCode();
        
        // Assign indexes to parameters in the method
        for (int i = 0; i < parameters.size(); i++) {
            mv.visitVarInsn(Opcodes.ILOAD, i);
        }
        
        body.generateCode(mv);
        mv.visitInsn(Opcodes.IRETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
    }
}

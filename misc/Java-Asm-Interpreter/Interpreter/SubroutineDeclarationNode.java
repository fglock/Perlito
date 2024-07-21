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
        throw new UnsupportedOperationException("SubroutineDeclarationNode cannot be evaluated directly.");
    }

    @Override
    public void generateCode(ClassWriter cw) {
        String methodDescriptor = "(";
        for (String parameter : parameters) {
            methodDescriptor += "I"; // Assuming all parameters are integers
        }
        methodDescriptor += ")I";

        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, name, methodDescriptor, null, null);
        mv.visitCode();

        // Generate code for the body
        body.generateCode(mv);

        // Return the result
        mv.visitInsn(Opcodes.IRETURN);

        // Define the max stack and local variables
        mv.visitMaxs(1, parameters.size() + 1); // 1 for stack, parameters.size() for locals
        mv.visitEnd();
    }
}


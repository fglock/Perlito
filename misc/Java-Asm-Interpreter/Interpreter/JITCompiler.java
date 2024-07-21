import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.util.List;

public class JITCompiler {
    public static void compileAndRun(Parser parser) {
        List<Node> statements = parser.parse();

        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "CompiledExpression", null, "java/lang/Object", null);

        // Generate static subroutine methods
        for (Node statement : statements) {
            if (statement instanceof MethodDefiningNode) {
                ((MethodDefiningNode) statement).generateCode(cw);
            }
        }

        // Generate main method
        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
        mv.visitCode();
        for (Node statement : statements) {
            if (statement instanceof CodeGeneratingNode) {
                ((CodeGeneratingNode) statement).generateCode(mv);
            }
        }
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();

        cw.visitEnd();

        byte[] bytecode = cw.toByteArray();

        DynamicClassLoader loader = new DynamicClassLoader();
        Class<?> compiledClass = loader.defineClass("CompiledExpression", bytecode);

        try {
            compiledClass.getMethod("main", String[].class).invoke(null, (Object) new String[]{});
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    static class DynamicClassLoader extends ClassLoader {
        public Class<?> defineClass(String name, byte[] bytecode) {
            return defineClass(name, bytecode, 0, bytecode.length);
        }
    }
}


import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.util.List;

public class JITCompiler {
    public static void compileAndRun(List<Node> statements) throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "CompiledExpression", null, "java/lang/Object", null);

        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
        mv.visitCode();

        for (Node statement : statements) {
            statement.generateCode(mv);
        }

        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(0, 0);
        mv.visitEnd();

        cw.visitEnd();

        byte[] bytecode = cw.toByteArray();

        CustomClassLoader loader = new CustomClassLoader();
        Class<?> compiledClass = loader.defineClass("CompiledExpression", bytecode);
        compiledClass.getMethod("main", String[].class).invoke(null, (Object) new String[0]);
    }
}

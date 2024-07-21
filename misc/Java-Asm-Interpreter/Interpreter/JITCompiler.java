import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;

import java.lang.reflect.Method;
import java.util.List;

public class JITCompiler {
    public static int compileAndRun(List<Node> nodes) throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "CompiledExpression", null, "java/lang/Object", null);

        for (Node node : nodes) {
            if (node instanceof MethodDefiningNode) {
                ((MethodDefiningNode) node).generateCode(cw);
            } else {
                throw new UnsupportedOperationException(node.getClass().getSimpleName() + " cannot generate code with MethodVisitor");
            }
        }

        cw.visitEnd();

        byte[] bytecode = cw.toByteArray();
        CustomClassLoader loader = new CustomClassLoader();
        Class<?> compiledClass = loader.defineClass("CompiledExpression", bytecode);
        Method method = compiledClass.getMethod("main");

        return (int) method.invoke(null);
    }

    static class CustomClassLoader extends ClassLoader {
        public Class<?> defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }
}

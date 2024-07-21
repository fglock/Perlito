import org.objectweb.asm.*;

import java.util.HashMap;
import java.util.Map;

public class JITCompiler {
    private static final Map<String, SubroutineDeclarationNode> subroutines = new HashMap<>();

    public static void compileAndRun(Parser parser) {
        Node root = parser.parse();
        if (root instanceof SubroutineDeclarationNode) {
            SubroutineDeclarationNode subroutine = (SubroutineDeclarationNode) root;
            subroutines.put(subroutine.getName(), subroutine);
        } else {
            compileAndRun(root);
        }
    }

    private static void compileAndRun(Node node) {
        ClassWriter cw = new ClassWriter(0);
        cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, "CompiledExpression", null, "java/lang/Object", null);

        MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "compiledMethod", "(I)I", null, null);
        mv.visitCode();
        node.generateCode(mv);
        mv.visitMaxs(1, 1);
        mv.visitEnd();

        for (SubroutineDeclarationNode subroutine : subroutines.values()) {
            subroutine.generateCode(cw);
        }

        cw.visitEnd();

        byte[] bytecode = cw.toByteArray();
        CustomClassLoader loader = new CustomClassLoader();
        Class<?> compiledClass = loader.defineClass("CompiledExpression", bytecode);
        try {
            java.lang.reflect.Method method = compiledClass.getMethod("compiledMethod", int.class);
            int result = (int) method.invoke(null, 0);
            System.out.println("Result: " + result);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static class CustomClassLoader extends ClassLoader {
        public Class<?> defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }
}


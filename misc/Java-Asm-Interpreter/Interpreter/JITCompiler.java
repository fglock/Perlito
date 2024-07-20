import org.objectweb.asm.*;

import java.lang.reflect.Method;

import static org.objectweb.asm.Opcodes.*;

public class JITCompiler {
    public static int compileAndRun(Node node) throws Exception {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        MethodVisitor mv;

        cw.visit(V1_8, ACC_PUBLIC + ACC_SUPER, "CompiledExpression", null, "java/lang/Object", null);

        // Default constructor
        mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();

        // main method
        mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
        mv.visitCode();

        // Call the compiled method and print the result
        mv.visitMethodInsn(INVOKESTATIC, "CompiledExpression", "compiledMethod", "()I", false);
        mv.visitVarInsn(ISTORE, 1);
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
        mv.visitVarInsn(ILOAD, 1);
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false);

        mv.visitInsn(RETURN);
        mv.visitMaxs(2, 2);
        mv.visitEnd();

        // Compiled method
        mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "compiledMethod", "()I", null, null);
        mv.visitCode();

        // Generate the bytecode for the expression
        node.generateCode(mv);
        mv.visitInsn(IRETURN);

        mv.visitMaxs(0, 0);
        mv.visitEnd();

        cw.visitEnd();

        // Load and execute the compiled class
        byte[] bytecode = cw.toByteArray();
        Class<?> compiledClass = new DynamicClassLoader().defineClass("CompiledExpression", bytecode);
        Method compiledMethod = compiledClass.getMethod("compiledMethod");
        return (int) compiledMethod.invoke(null);
    }

    static class DynamicClassLoader extends ClassLoader {
        public Class<?> defineClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }
}


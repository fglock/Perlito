package misc.Java_asm;

import java.io.FileOutputStream;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;

import static org.objectweb.asm.Opcodes.*;

public class AsmTest {

	public static void main(String[] args) throws Exception {
		ClassWriter cw=new ClassWriter(0);
		
		cw.visit(V1_6, ACC_PUBLIC+ACC_SUPER, "misc/Java_asm/AsmTestOut", null, "java/lang/Object", null);
	
		//default constructor
		{
			MethodVisitor mv=cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
			mv.visitCode();
			mv.visitVarInsn(ALOAD, 0); //load the first local variable: this
			mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
			mv.visitInsn(RETURN);
			mv.visitMaxs(1,1);
			mv.visitEnd();
		}
		
		//main method
		{
			MethodVisitor mv=cw.visitMethod(ACC_PUBLIC+ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
			mv.visitCode();
			mv.visitFieldInsn(GETSTATIC,"java/lang/System", "out", "Ljava/io/PrintStream;"); //put System.out to operand stack
			mv.visitLdcInsn("Hello"); //load const "Hello" from const_pool, and put onto the operand stack
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
			mv.visitInsn(RETURN);
			mv.visitMaxs(2,1);
			mv.visitEnd();
		}
		cw.visitEnd();
		
		//save bytecode into disk
		FileOutputStream out=new FileOutputStream("misc/Java_asm/AsmTestOut.class");
		out.write(cw.toByteArray());
		out.close();
	}
}

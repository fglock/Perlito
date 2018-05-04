//
// ASMifier experiments
//
//
//  $ cat > HelloWorld.java
//  public class HelloWorld {
//      public static void myMethod() {
//          System.out.println("Hello, World");
//      }
//      public static void main(String[] args) {
//          myMethod();
//      }
//  }
//  
//  $ javac HelloWorld.java
//  $ java -classpath asm-6.1.1.jar:asm-util-6.1.jar org.objectweb.asm.util.ASMifier HelloWorld.class > HelloWorldDump.java
//
//  - add java.lang.reflect imports
//  - add reflection calls
//
//  $ cp misc/Java_asm/HelloWorldDump.java .
//  $ javac -cp asm-6.1.1.jar HelloWorldDump.java 
//  $ java -cp asm-6.1.1.jar:. HelloWorldDump
//


import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.TypePath;

import java.lang.reflect.Method;

public class HelloWorldDump implements Opcodes {

public static byte[] dump () throws Exception {

ClassWriter classWriter = new ClassWriter(0);
FieldVisitor fieldVisitor;
MethodVisitor methodVisitor;
AnnotationVisitor annotationVisitor0;

classWriter.visit(V9, ACC_PUBLIC | ACC_SUPER, "HelloWorld", null, "java/lang/Object", null);

classWriter.visitSource("HelloWorld.java", null);

{
methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
methodVisitor.visitCode();
Label label0 = new Label();
methodVisitor.visitLabel(label0);
methodVisitor.visitLineNumber(1, label0);
methodVisitor.visitVarInsn(ALOAD, 0);
methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
methodVisitor.visitInsn(RETURN);
methodVisitor.visitMaxs(1, 1);
methodVisitor.visitEnd();
}

{
methodVisitor = classWriter.visitMethod(ACC_PUBLIC | ACC_STATIC, "myMethod", "()V", null, null);
methodVisitor.visitCode();
Label label0 = new Label();
methodVisitor.visitLabel(label0);
methodVisitor.visitLineNumber(3, label0);
methodVisitor.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
methodVisitor.visitLdcInsn("Hello, World");
methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false);
Label label1 = new Label();
methodVisitor.visitLabel(label1);
methodVisitor.visitLineNumber(4, label1);
methodVisitor.visitInsn(RETURN);
methodVisitor.visitMaxs(2, 0);
methodVisitor.visitEnd();
}

classWriter.visitEnd();

return classWriter.toByteArray();
}

    public static void main(String[] args) {
        try {
            dump();

            Class class1 = Class.forName("HelloWorld");
            System.out.println("got class " + class1.toString());

            Method method5 = Class.forName("HelloWorld").getMethod("myMethod", new Class[]{});

            System.out.println("got method " + method5.toString());

            method5.invoke(null);

        }
        catch (Exception e) {
            System.out.println("got error " + e.toString());
        }
    }

}

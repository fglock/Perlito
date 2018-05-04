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

// Java compiler and Classloader
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.net.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;


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
            DynamicClassLoader classLoader = new DynamicClassLoader( new String().getClass().getClassLoader() );

            String className = "HelloWorld";

            byte[] myBytecode = dump();
            classLoader.customCompiledCode.put(className, new CompiledCode(className, myBytecode));

            Class<?> class1 = classLoader.loadClass(className);
            System.out.println("got class " + class1.toString());

            Method method5 = class1.getMethod("myMethod", new Class[]{});
            System.out.println("got method " + method5.toString());

            method5.invoke(null);
        }
        catch (Exception e) {
            System.out.println("got error " + e.toString());
        }
    }

}

class CompiledCode extends SimpleJavaFileObject {
    private String className;
    private byte[] classByteCode;

    public CompiledCode(String className, byte[] byteCode) throws Exception {
        super(new URI(className), Kind.CLASS);
        this.className = className;
        this.classByteCode = byteCode;
    }
    
    public String getClassName() {
        return className;
    }

    public byte[] getByteCode() {
        // return baos.toByteArray();
        return this.classByteCode;
    }
}

class DynamicClassLoader extends ClassLoader {
    public Map<String, CompiledCode> customCompiledCode = new HashMap<String, CompiledCode>();

    public DynamicClassLoader(ClassLoader parent) {
        super(parent);
    }

    public void addCode(CompiledCode cc) {
        // System.out.println("DynamicClassLoader.addCode: name=" + cc.getName());
        customCompiledCode.put(cc.getName(), cc);
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        // System.out.println("DynamicClassLoader.findClass: name=" + name);
        CompiledCode cc = customCompiledCode.get(name);
        if (cc == null) {
            return super.findClass(name);
        }
        byte[] byteCode = cc.getByteCode();
        return defineClass(name, byteCode, 0, byteCode.length);
    }
}



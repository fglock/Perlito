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
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;


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
            PlJavaCompiler.init();


            // Class<?> class5 = compileClassInMemory(
            //     className,
            //     cls5
            // );

            String className = "HelloWorld";

            byte[] myBytecode = dump();
            PlJavaCompiler.classLoader.customCompiledCode.put(className, new CompiledCode(className, myBytecode));

            // Class class1 = Class.forName(className);
            Class class1 = PlJavaCompiler.classLoader.loadClass(className);
            System.out.println("got class " + class1.toString());

            Method method5 = Class.forName(className).getMethod("myMethod", new Class[]{});

            System.out.println("got method " + method5.toString());

            method5.invoke(null);

        }
        catch (Exception e) {
            System.out.println("got error " + e.toString());
        }
    }

}

class PlJavaCompiler {
    private PlJavaCompiler() {} // defined so class can't be instantiated.

    static ArrayList<SourceCode> compilationUnits;
    static ExtendedStandardJavaFileManager fileManager;
    static DynamicClassLoader classLoader;
    static JavaCompiler javac;
    static List<String> optionList;

    public static void init() throws Exception
    {
        javac = ToolProvider.getSystemJavaCompiler();
        classLoader = new DynamicClassLoader( new String().getClass().getClassLoader() );
        compilationUnits = new ArrayList<SourceCode>();
        optionList = new ArrayList<String>();
        // set compiler's classpath to be same as the runtime's
        StringBuilder cp = new StringBuilder();
        int cpCount = 0;
        try {
            for (URL url : ((URLClassLoader) (Thread.currentThread().getContextClassLoader())).getURLs()) {
                if (cpCount++ != 0) {
                    cp.append(":");
                }
                cp.append(url.getFile());
            }
        }
        catch (Exception e) {
        }
        String systemCp = System.getProperty("java.class.path");
        if (! systemCp.equals("")) {
            if (cpCount++ != 0) {
                cp.append(":");
            }
            cp.append(systemCp);
        }
        optionList.addAll(Arrays.asList("-classpath", cp.toString()));
        fileManager = new ExtendedStandardJavaFileManager(
                    javac.getStandardFileManager(null, null, null), classLoader);
    }

    // static Class<?> compileClassInMemory(String className, String classSourceCode) throws Exception
    // {
    //     SourceCode sourceCodeObj = new SourceCode(className, classSourceCode);
    //     // System.out.println("PlJavaCompiler.compileClassInMemory: name=" + className);
    //     classLoader.customCompiledCode.put(className, new CompiledCode(className));
    //     compilationUnits.set(0, sourceCodeObj);

    //     // run the compiler
    //     JavaCompiler.CompilationTask task = javac.getTask(null, fileManager,
    //             null, optionList, null, compilationUnits);
    //     boolean result = task.call();
    //     if (!result)
    //         throw new RuntimeException("Unknown error during compilation.");
    //     return classLoader.loadClass(className);
    // }

}

class ExtendedStandardJavaFileManager extends ForwardingJavaFileManager<JavaFileManager> {
    private DynamicClassLoader cl;

    protected ExtendedStandardJavaFileManager(JavaFileManager fileManager, DynamicClassLoader cl) {
        super(fileManager);
        this.cl = cl;
    }

    @Override
    public JavaFileObject getJavaFileForOutput(JavaFileManager.Location location, String className, JavaFileObject.Kind kind, FileObject sibling) throws IOException {
        System.out.println("ExtendedStandardJavaFileManager.getJavaFileForOutput: name=" + className);
        CompiledCode cc = cl.customCompiledCode.get(className);
        if (cc != null) {
            return cc;
        }
        // source file not found for this output class: this is ok, because we can have a class like 'PlEval$1'
        // System.out.println("ExtendedStandardJavaFileManager.getJavaFileForOutput: create name=" + className);
        try {
            cc = new CompiledCode(className);
        }
        catch(Exception e) {
            throw new FileNotFoundException("Error creating output file for class " + className );
        }
        cl.customCompiledCode.put(className, cc);
        return cc;
    }

    @Override
    public ClassLoader getClassLoader(JavaFileManager.Location location) {
        return cl;
    }
}

class CompiledCode extends SimpleJavaFileObject {
    private ByteArrayOutputStream baos = new ByteArrayOutputStream();
    private String className;
    private byte[] classByteCode;

    public CompiledCode(String className) throws Exception {
        super(new URI(className), Kind.CLASS);
        this.className = className;
    }
    public CompiledCode(String className, byte[] byteCode) throws Exception {
        super(new URI(className), Kind.CLASS);
        this.className = className;
        this.classByteCode = byteCode;
    }
    
    public String getClassName() {
        // System.out.println("CompiledCode.getClassName: name=" + className);
        return className;
    }

    @Override
    public OutputStream openOutputStream() throws IOException {
        // System.out.println("CompiledCode.openOutputStream()");
        return baos;
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

class SourceCode extends SimpleJavaFileObject {
    private String contents = null;
    private String className;

    public SourceCode(String className, String contents) throws Exception {
        super(URI.create("string:///" + className.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
        this.contents = contents;
        this.className = className;
    }

    public String getClassName() {
        return className;
    }

    public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
        return contents;
    }
}



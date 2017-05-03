//
//
//  $ cp ../../perlito5.jar .
//  $ javac -cp perlito5.jar JavaCompiler5.java
//  $ java  -cp '.:perlito5.jar' JavaCompiler5
//
// Credits:
//
// http://udn.yyuap.com/doc/jdk6-api-zh/javax/tools/JavaCompiler.html         
//  * idea to reuse the same file manager to allow caching of jar files
// https://github.com/turpid-monkey/InMemoryJavaCompiler
// https://github.com/trung/InMemoryJavaCompiler
//  * provided a working example
//  * Apache License, Version 2.0 - http://www.apache.org/licenses/LICENSE-2.0.txt
// http://stackoverflow.com/questions/1563909/how-to-set-classpath-when-i-use-javax-tools-javacompiler-compile-the-source
//  * set classpath
//

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import org.perlito.Perlito5.*;

public class JavaCompiler5
{
    static ArrayList<SourceCode> compilationUnits;
    static ExtendedStandardJavaFileManager fileManager;
    static DynamicClassLoader classLoader;
    static JavaCompiler javac;

    static Class<?> compileClassInMemory(String className, String classSourceCode) throws Exception
    {
        SourceCode sourceCodeObj = new SourceCode(className, classSourceCode);
        classLoader.customCompiledCode.put(className, new CompiledCode(className));
        if (fileManager == null) {
            // initializing the file manager
            compilationUnits.add(sourceCodeObj);
            fileManager = new ExtendedStandardJavaFileManager(
                    javac.getStandardFileManager(null, null, null), classLoader);
        }
        else {
            // reusing the file manager; replace the source code
            compilationUnits.set(1, sourceCodeObj);
        }

        List<String> optionList = new ArrayList<String>();
        // set compiler's classpath to be same as the runtime's
        optionList.addAll(Arrays.asList("-classpath",System.getProperty("java.class.path")));
        // optionList.addAll(Arrays.asList("-classpath", "."));
        optionList.addAll(Arrays.asList("-classpath", "perlito5.jar"));

        // run the compiler
        JavaCompiler.CompilationTask task = javac.getTask(null, fileManager,
                null, optionList, null, compilationUnits);
        boolean result = task.call();
        if (!result)
            throw new RuntimeException("Unknown error during compilation.");
        return classLoader.loadClass(className);
    }

    public static void main(String[] args) throws Exception
    {
        javac = ToolProvider.getSystemJavaCompiler();
        classLoader = new DynamicClassLoader(ClassLoader.getSystemClassLoader());
        compilationUnits = new ArrayList<SourceCode>();

        StringBuffer source3 = new StringBuffer();
        source3.append(" import org.perlito.Perlito5.*;");
        source3.append(" public class Adder {");
        source3.append("     public Adder() {");
        source3.append("     }");
        source3.append("     public static PlObject createObject() {");
        source3.append("        return new PlString(\"HERE!!!\");");
        source3.append("     }");
        source3.append("     public static PlObject doSomething(PlObject o) {");
        source3.append("         return new PlString(\"[[\" + o.toString() + \"]]\");");
        source3.append("     }");
        source3.append(" }");
        String cls3 = source3.toString();
        String name3 = "Adder";
        Class<?> class3 = compileClassInMemory(
            name3,
            cls3
        );

        Method method3 = class3.getMethod("createObject");
        PlObject aaa = (PlObject)method3.invoke(null);
        System.out.println(aaa);

        Method method4 = class3.getMethod("doSomething", new Class[]{PlObject.class});
        PlObject bbb = (PlObject)method4.invoke(null, new PlString("TEST"));
        System.out.println(bbb);
    }
}

class ExtendedStandardJavaFileManager extends ForwardingJavaFileManager<JavaFileManager> {
    private DynamicClassLoader cl;

    protected ExtendedStandardJavaFileManager(JavaFileManager fileManager, DynamicClassLoader cl) {
        super(fileManager);
        this.cl = cl;
    }

    @Override
    public JavaFileObject getJavaFileForOutput(JavaFileManager.Location location, String className, JavaFileObject.Kind kind, FileObject sibling) throws IOException {
        CompiledCode cc = cl.customCompiledCode.get(className);
        if (cc != null) {
            return cc;
        }
        throw new FileNotFoundException("Missing source code for class " + className );
    }

    @Override
    public ClassLoader getClassLoader(JavaFileManager.Location location) {
        return cl;
    }
}

class CompiledCode extends SimpleJavaFileObject {
    private ByteArrayOutputStream baos = new ByteArrayOutputStream();
    private String className;

    public CompiledCode(String className) throws Exception {
        super(new URI(className), Kind.CLASS);
        this.className = className;
    }
    
    public String getClassName() {
        return className;
    }

    @Override
    public OutputStream openOutputStream() throws IOException {
        return baos;
    }

    public byte[] getByteCode() {
        return baos.toByteArray();
    }
}

class DynamicClassLoader extends ClassLoader {
    public Map<String, CompiledCode> customCompiledCode = new HashMap<String, CompiledCode>();

    public DynamicClassLoader(ClassLoader parent) {
        super(parent);
    }

    public void addCode(CompiledCode cc) {
        customCompiledCode.put(cc.getName(), cc);
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
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


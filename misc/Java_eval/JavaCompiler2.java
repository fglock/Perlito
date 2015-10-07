// https://github.com/trung/InMemoryJavaCompiler
//             <name>Apache License, Version 2.0</name>
//              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

import org.perlito5.PlInterface;

class Adder implements PlInterface {
    public int add(int x, int y) {
        return x + y;
    }
}

public class JavaCompiler2
{
    static JavaCompiler javac = ToolProvider.getSystemJavaCompiler();
    static DynamicClassLoader cl = new DynamicClassLoader(ClassLoader.getSystemClassLoader());
    static String className = "InMemoryClass";

    public static void main(String[] args) throws Exception
    {
        CompiledCode compiledCode = new CompiledCode(className);

        Adder aaa = new Adder();

        StringBuffer sourceCode = new StringBuffer();
        sourceCode.append("import org.perlito5.PlInterface;\n");
        sourceCode.append("public class " + className + " {\n");
        sourceCode.append("   public static void hello(PlInterface x) { System.out.print(\"hello \" + x.add(3,4) + \"\\n\"); }");
        sourceCode.append("}");

        for (int i = 0; i < 1000 ; i++) {
            SourceCode sourceCodeObj = new SourceCode(className, sourceCode.toString());
            Iterable<? extends JavaFileObject> compilationUnits = Arrays.asList(sourceCodeObj);
            ExtendedStandardJavaFileManager fileManager = new ExtendedStandardJavaFileManager(javac.getStandardFileManager(null, null, null), compiledCode, cl);
            JavaCompiler.CompilationTask task = javac.getTask(null, fileManager, null, null, null, compilationUnits);
            boolean result = task.call();
            Class<?> helloClass = cl.loadClass(className);
    
            // System.out.println("Loaded class name: " + helloClass.getName());
    
            // Getting the target method from the loaded class and invoke it using its name
            Method method = helloClass.getMethod("hello", new Class[]{PlInterface.class});
            // System.out.println("Invoked method name: " + method.getName());
    
            method.invoke(null, aaa);
        }
    }
}


class SourceCode extends SimpleJavaFileObject {
    private String contents = null;

    public SourceCode(String className, String contents) throws Exception {
        super(URI.create("string:///" + className.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
        this.contents = contents;
    }

    public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
        return contents;
    }
}

class CompiledCode extends SimpleJavaFileObject {
    private ByteArrayOutputStream baos = new ByteArrayOutputStream();

    public CompiledCode(String className) throws Exception {
        super(new URI(className), Kind.CLASS);
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
    private Map<String, CompiledCode> customCompiledCode = new HashMap<String, CompiledCode>();

    public DynamicClassLoader(ClassLoader parent) {
        super(parent);
    }

    public void setCode(CompiledCode cc) {
        customCompiledCode.put(cc.getName(), cc);
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {

        System.out.println("DynamicClassLoader get compiled code: " + name);

        CompiledCode cc = customCompiledCode.get(name);
        if (cc == null) {
            return super.findClass(name);
        }
        byte[] byteCode = cc.getByteCode();
        return defineClass(name, byteCode, 0, byteCode.length);
    }
}

class ExtendedStandardJavaFileManager extends ForwardingJavaFileManager<JavaFileManager> {
    private CompiledCode compiledCode;
    private DynamicClassLoader cl;

    /**
     * Creates a new instance of ForwardingJavaFileManager.
     *
     * @param fileManager delegate to this file manager
     * @param cl
     */
    public ExtendedStandardJavaFileManager(JavaFileManager fileManager, CompiledCode compiledCode, DynamicClassLoader cl) {
        super(fileManager);
        this.compiledCode = compiledCode;
        this.cl = cl;
        this.cl.setCode(compiledCode);
    }

    @Override
    public JavaFileObject getJavaFileForOutput(JavaFileManager.Location location, String className, JavaFileObject.Kind kind, FileObject sibling) throws IOException {
        // System.out.println("ExtendedStandardJavaFileManager file for output location: " + location.getName());
        return compiledCode;
    }

    @Override
    public ClassLoader getClassLoader(JavaFileManager.Location location) {
        // System.out.println("ExtendedStandardJavaFileManager class loader location: " + location.getName());
        return cl;
    }
}


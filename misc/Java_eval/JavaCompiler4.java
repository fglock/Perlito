// https://github.com/trung/InMemoryJavaCompiler
//             <name>Apache License, Version 2.0</name>
//              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
// https://github.com/turpid-monkey/InMemoryJavaCompiler
// and others
//

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class JavaCompiler4
{
    static Class<?> compile_class(
            String className,
            String classSourceCode,
            ArrayList<SourceCode> compilationUnits,
            CompiledCode[] code,
            ExtendedStandardJavaFileManager fileManager,
            DynamicClassLoader classLoader,
            JavaCompiler javac
        ) throws Exception
    {
        SourceCode sourceCodeObj = new SourceCode(className, classSourceCode);
		CompiledCode compiledCodeObj = new CompiledCode(className);
		compilationUnits.set(1, sourceCodeObj);
		code[1] = compiledCodeObj;
		classLoader.addCode(compiledCodeObj);
		JavaCompiler.CompilationTask task2 = javac.getTask(null, fileManager,
				null, null, null, compilationUnits);
		boolean result = task2.call();
		if (!result)
			throw new RuntimeException("Unknown error during compilation.");
        return classLoader.loadClass(className);
    }

    public static void main(String[] args) throws Exception
    {
        StringBuffer source4 = new StringBuffer();
        source4.append("package org.perlito5;");
        source4.append("public interface PlInterface {");
        source4.append("    int add(int x, int y);");
        source4.append("}");
        String cls4 = source4.toString();
        String name4 = "org.perlito5.PlInterface";

        StringBuffer source3 = new StringBuffer();
        source3.append("package org.perlito5;");
        source3.append("public class Adder implements PlInterface {");
        source3.append("    public Adder() {");
        source3.append("    }");
        source3.append("    public int add(int x, int y) {");
        source3.append("        return x + y;");
        source3.append("    }");
        source3.append("    public static Adder getAdder() {");
        source3.append("        return new Adder();");
        source3.append("    }");
        source3.append("}");
        String cls3 = source3.toString();
        String name3 = "org.perlito5.Adder";

	    JavaCompiler javac = ToolProvider.getSystemJavaCompiler();
	    DynamicClassLoader classLoader = new DynamicClassLoader(ClassLoader.getSystemClassLoader());
		ArrayList<SourceCode> compilationUnits3 = new ArrayList<SourceCode>();
        compilationUnits3.add(new SourceCode(name4, cls4));
        compilationUnits3.add(new SourceCode(name3, cls3));
		CompiledCode[] code3 = new CompiledCode[compilationUnits3.size()];
		code3[0] = new CompiledCode(name4);
		code3[1] = new CompiledCode(name3);
		ExtendedStandardJavaFileManager fileManager = new ExtendedStandardJavaFileManager(
				javac.getStandardFileManager(null, null, null), classLoader, code3);
		JavaCompiler.CompilationTask task = javac.getTask(null, fileManager,
				null, null, null, compilationUnits3);
		boolean result = task.call();
		if (!result)
			throw new RuntimeException("Unknown error during compilation.");
        Class<?> helloClass3 = classLoader.loadClass("org.perlito5.Adder");

        System.out.println("Methods:");
        for ( Method method : helloClass3.getMethods() ) {
            System.out.println( method.toString() );
        }
        System.out.println("Constructors:");
        for ( Constructor constructor : helloClass3.getConstructors() ) {
            System.out.println( constructor.toString() );
        }
        Method method3 = helloClass3.getMethod("getAdder", new Class[]{});
        Object aaa = method3.invoke(null);

        StringBuffer sourceCode = new StringBuffer();
        sourceCode.append("package org.perlito5;");
        sourceCode.append("public class HelloClass {\n");
        sourceCode.append("   public static void hello(Object x) { System.out.print(\"hello \" + ((PlInterface)x).add(3,4) + \"\\n\"); }");
        sourceCode.append("}");
        String cls1 = sourceCode.toString();
        Class<?> helloClass = compile_class(
            "org.perlito5.HelloClass",
            cls1,
            compilationUnits3,
            code3,
            fileManager,
            classLoader,
            javac
        );
 
        // Getting the target method from the loaded class and invoke it using its name
        Method method = helloClass.getMethod("hello", new Class[]{Object.class});

        // Adder aaa = new Adder();
        method.invoke(null, aaa);
    }
}

class ExtendedStandardJavaFileManager extends
		ForwardingJavaFileManager<JavaFileManager> {

	private CompiledCode[] compiledCode;
	private DynamicClassLoader cl;

	/**
	 * Creates a new instance of ForwardingJavaFileManager.
	 *
	 * @param fileManager
	 *            delegate to this file manager
	 * @param cl
	 */
	protected ExtendedStandardJavaFileManager(JavaFileManager fileManager,
			DynamicClassLoader cl, CompiledCode... compiledCode) {
		super(fileManager);
		this.compiledCode = compiledCode;
		this.cl = cl;
		for (CompiledCode code : compiledCode) {
			this.cl.addCode(code);
		}
	}

	@Override
    public JavaFileObject getJavaFileForOutput(JavaFileManager.Location location, String className, JavaFileObject.Kind kind, FileObject sibling) throws IOException {
    	for (CompiledCode code : compiledCode)
    	{
    		if (code.getClassName().equals(className)) return code;
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

    private Map<String, CompiledCode> customCompiledCode = new HashMap<String, CompiledCode>();

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
		super(URI.create("string:///" + className.replace('.', '/')
				+ Kind.SOURCE.extension), Kind.SOURCE);
		this.contents = contents;
		this.className = className;
	}

	public String getClassName() {
		return className;
	}

	public CharSequence getCharContent(boolean ignoreEncodingErrors)
			throws IOException {
		return contents;
	}
}


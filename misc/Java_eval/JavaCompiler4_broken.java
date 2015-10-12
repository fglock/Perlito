// https://github.com/trung/InMemoryJavaCompiler
//             <name>Apache License, Version 2.0</name>
//              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
// https://github.com/turpid-monkey/InMemoryJavaCompiler
//

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URI;
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


public class JavaCompiler4_broken
{
    public static void main(String[] args) throws Exception
    {

	    JavaCompiler javac = ToolProvider.getSystemJavaCompiler();
	    DynamicClassLoader classLoader = new DynamicClassLoader(ClassLoader.getSystemClassLoader());


        StringBuffer source4 = new StringBuffer();
        source4.append("package org.perlito5;");
        source4.append("public interface PlInterface {");
        source4.append("    int add(int x, int y);");
        source4.append("}");
        String cls4 = source4.toString();

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



	    Map<String, SourceCode> clazzCode3 = new HashMap<String, SourceCode>();
		clazzCode3.put("org.perlito5.PlInterface", new SourceCode("org.perlito5.PlInterface", cls4));
		clazzCode3.put("org.perlito5.Adder", new SourceCode("org.perlito5.Adder", cls3));


		Collection<SourceCode> compilationUnits3 = clazzCode3.values();
		CompiledCode[] code3;
		code3 = new CompiledCode[compilationUnits3.size()];
		Iterator<SourceCode> iter3 = compilationUnits3.iterator();
		for (int i=0; i<code3.length; i++)
		{
			code3[i] = new CompiledCode(iter3.next().getClassName());
		}
		
		ExtendedStandardJavaFileManager fileManager = new ExtendedStandardJavaFileManager(
				javac.getStandardFileManager(null, null, null), classLoader, code3);
		JavaCompiler.CompilationTask task = javac.getTask(null, fileManager,
				null, null, null, compilationUnits3);
		boolean result = task.call();
		if (!result)
			throw new RuntimeException("Unknown error during compilation.");
		Map<String, Class<?>> classes = new HashMap<String, Class<?>>();
		for (String className : clazzCode3.keySet()) {
			classes.put(className, classLoader.loadClass(className));
		}
        Map<String,Class<?>> compiled3 = classes;



        Class<?> helloClass3 = compiled3.get("org.perlito5.Adder");

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

        // Constructor constructor3 = helloClass3.getConstructor(new Class[]{});

        // String cls2 = // "package org.perlito5;\n"
        //             "public interface PlInterface { int add(int x, int y); }";

        StringBuffer sourceCode = new StringBuffer();
        // sourceCode.append("package org.mdkt;\n");
        // sourceCode.append("import org.perlito5.PlInterface;\n");
        sourceCode.append("package org.perlito5;");
        sourceCode.append("public class HelloClass {\n");
        sourceCode.append("   public static void hello(Object x) { System.out.print(\"hello \" + ((PlInterface)x).add(3,4) + \"\\n\"); }");
        sourceCode.append("}");
        String cls1 = sourceCode.toString();

	    Map<String, SourceCode> clazzCode = new HashMap<String, SourceCode>();
		clazzCode.put("org.perlito5.HelloClass", new SourceCode("org.perlito5.HelloClass", cls1));


		Collection<SourceCode> compilationUnits = clazzCode.values();
		CompiledCode[] code;
		code = new CompiledCode[compilationUnits.size()];
		Iterator<SourceCode> iter = compilationUnits.iterator();
		for (int i=0; i<code.length; i++)
		{
			code[i] = new CompiledCode(iter.next().getClassName());
		}

		// ExtendedStandardJavaFileManager fileManager = new ExtendedStandardJavaFileManager(
		// 		javac.getStandardFileManager(null, null, null), classLoader, code);
		JavaCompiler.CompilationTask task2 = javac.getTask(null, fileManager,
				null, null, null, compilationUnits);
		result = task2.call();
		if (!result)
			throw new RuntimeException("Unknown error during compilation.");
		classes = new HashMap<String, Class<?>>();
		for (String className : clazzCode.keySet()) {
			classes.put(className, classLoader.loadClass(className));
		}
        Map<String,Class<?>> compiled = classes;




        Class<?> helloClass = compiled.get("org.perlito5.HelloClass");

        // System.out.println("Loaded class name: " + helloClass.getName());

        // Getting the target method from the loaded class and invoke it using its name
        Method method = helloClass.getMethod("hello", new Class[]{Object.class});
        // System.out.println("Invoked method name: " + method.getName());

        // Adder aaa = new Adder();

        method.invoke(null, aaa);
    }
}


/**
 * Created by trung on 5/3/15.
 * Edited by turpid-monkey on 9/25/15, added support for multiple, dependent compile units.
 */
class InMemoryJavaCompiler {
	JavaCompiler javac;
	DynamicClassLoader classLoader;

	Map<String, SourceCode> clazzCode = new HashMap<String, SourceCode>();

	public InMemoryJavaCompiler(ClassLoader parent) {
		this(ToolProvider.getSystemJavaCompiler(), parent);
	}

	public InMemoryJavaCompiler(JavaCompiler javac, ClassLoader parent) {
		this.javac = javac;
		this.classLoader = new DynamicClassLoader(parent);
	}

	public InMemoryJavaCompiler() {
		this(ToolProvider.getSystemJavaCompiler(), ClassLoader
				.getSystemClassLoader());
	}

	public void addSource(String className, String sourceCodeInText)
			throws Exception {
		clazzCode.put(className, new SourceCode(className, sourceCodeInText));
	}

	public Map<String, Class<?>> compileAll() throws Exception {
		Collection<SourceCode> compilationUnits = clazzCode.values();
		CompiledCode[] code;
		
		code = new CompiledCode[compilationUnits.size()];
		Iterator<SourceCode> iter = compilationUnits.iterator();
		for (int i=0; i<code.length; i++)
		{
			code[i] = new CompiledCode(iter.next().getClassName());
		}
		
		ExtendedStandardJavaFileManager fileManager = new ExtendedStandardJavaFileManager(
				javac.getStandardFileManager(null, null, null), classLoader, code);
		JavaCompiler.CompilationTask task = javac.getTask(null, fileManager,
				null, null, null, compilationUnits);
		boolean result = task.call();
		if (!result)
			throw new RuntimeException("Unknown error during compilation.");
		Map<String, Class<?>> classes = new HashMap<String, Class<?>>();
		for (String className : clazzCode.keySet()) {
			classes.put(className, classLoader.loadClass(className));
		}
		return classes;
	}

	public static Class<?> compile(String className, String sourceCodeInText)
			throws Exception {
		InMemoryJavaCompiler comp = new InMemoryJavaCompiler();
		comp.addSource(className, sourceCodeInText);
		Map<String, Class<?>> clzzes = comp.compileAll();
		Class<?> result = clzzes.get(className);
		return result;
	}
	
	public DynamicClassLoader getClassLoader() {
		return classLoader;
	}
}


/**
 * Created by trung on 5/3/15.
 * Edited by turpid-monkey on 9/25/15, completed support for multiple compile units.
 */
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


/**
 * Created by trung on 5/3/15.
 */
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





/**
 * Created by trung on 5/3/15.
 */
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


/**
 * Created by trung on 5/3/15.
 */
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


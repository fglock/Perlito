package org.mdkt.compiler;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

/**
 * Created by trung on 5/3/15.
 * Edited by turpid-monkey on 9/25/15, added support for multiple, dependent compile units.
 */
public class InMemoryJavaCompiler {
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

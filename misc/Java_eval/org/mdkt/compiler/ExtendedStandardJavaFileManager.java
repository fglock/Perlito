package org.mdkt.compiler;

import java.io.FileNotFoundException;
import java.io.IOException;

import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;

/**
 * Created by trung on 5/3/15.
 * Edited by turpid-monkey on 9/25/15, completed support for multiple compile units.
 */
public class ExtendedStandardJavaFileManager extends
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

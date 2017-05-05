use v5;

package Perlito5::Java::JavaCompiler;
use strict;

sub emit_java_imports {
    return <<'EOT'
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

EOT
}

sub emit_java {
    return <<'EOT'

/****************************************************************************/
// Credits for the PlJavaCompiler idea:
//
// http://udn.yyuap.com/doc/jdk6-api-zh/javax/tools/JavaCompiler.html         
//  * idea to reuse the same file manager to allow caching of jar files
// https://github.com/turpid-monkey/InMemoryJavaCompiler
// https://github.com/trung/InMemoryJavaCompiler
//  * provided a working example
//  * Apache License, Version 2.0 - http://www.apache.org/licenses/LICENSE-2.0.txt
// http://stackoverflow.com/questions/1563909/how-to-set-classpath-when-i-use-javax-tools-javacompiler-compile-the-source
//  * set classpath
/****************************************************************************/

class PlJavaCompiler {
    private PlJavaCompiler() {} // defined so class can't be instantiated.

    static ArrayList<SourceCode> compilationUnits;
    static ExtendedStandardJavaFileManager fileManager;
    static DynamicClassLoader classLoader;
    static JavaCompiler javac;
    static Boolean initDone;
    static int invocationCount = 0;

    public static void init() throws Exception
    {
        // System.out.println("initializing Perlito5.LibPerl");
        // try {
        //     LibPerl.main( new String[]{} );
        // }
        // catch(Exception e) {
        //     System.out.println("Errors in main()");
        // }

        javac = ToolProvider.getSystemJavaCompiler();
        classLoader = new DynamicClassLoader(ClassLoader.getSystemClassLoader());
        compilationUnits = new ArrayList<SourceCode>();
    }

    public static PlObject eval_java_string(String source)
    {
        try {
            if (initDone == null) {
                PlJavaCompiler.init();
                System.out.println("eval_string: init");
                initDone = true;
            }

            // TODO - test local(); initialize local() stack if needed
            String className = "PlEval" + invocationCount;
            invocationCount++;

            StringBuffer source5 = new StringBuffer();
            source5.append("import org.perlito.Perlito5.*;\n");
            source5.append("public class " + className + " {\n");
            source5.append("    public " + className + "() {\n");
            source5.append("    }\n");
            source5.append("    public static PlObject runEval(int want) {\n");
            source5.append("        try {\n");
            source5.append("        " + source + "\n");
            source5.append("        }\n");
            source5.append("        catch(PlReturnException e) {\n");
            source5.append("            return e.ret;\n");
            source5.append("        }\n");
            source5.append("    }\n");
            source5.append("}\n");
            String cls5 = source5.toString();
            System.out.println("\neval_string:\n" + cls5 + "\n");

            // TODO - retrieve errors in Java->bytecode
            Class<?> class5 = compileClassInMemory(
                className,
                cls5
            );
            Method method5 = class5.getMethod("runEval", new Class[]{int.class});
            PlObject out = (org.perlito.Perlito5.PlObject)method5.invoke(null, PlCx.VOID);
            System.out.println("eval_string result: " + out.toString());
            return out;
        }
        catch(Exception e) {
            e.printStackTrace();
            String message = e.getMessage();
            System.out.println("Exception in eval_string: " + message);
            PlV.sset("main::@", new PlString(message));
        }
        return PlCx.UNDEF;
    }

    public static PlObject eval_perl_string(String source, String wantarray, int strict)
    {
        try {
            System.out.println("eval_string: enter");
            (new Throwable()).printStackTrace();

            PlV.sset("Perlito5::STRICT", new PlInt(strict));

            // # $m = Perlito5::Grammar::exp_stmts($source, 0);
            System.out.println("eval_string: calling Perlito5::Grammar::exp_stmts");
            PlObject[] ast = org.perlito.Perlito5.LibPerl.apply(
                "Perlito5::Grammar::exp_stmts",
                "{; " + source + " }"
            );

            PlObject[] out = LibPerl.apply( "Perlito5::Dumper::ast_dumper", ast[0].hget("capture") );
            System.out.println(out[0]);

            // # $ast->emit_java(0);
            PlObject outJava = org.perlito.Perlito5.PerlOp.call(
                ast[0].hget("capture").aget(0),
                "emit_java",
                new PlArray(new PlInt(0), new PlString(wantarray)),
                PlCx.SCALAR);
            // System.out.println("eval_string: " + outJava);
            return eval_java_string(outJava.toString());
        }
        catch(Exception e) {
            e.printStackTrace();
            String message = e.getMessage();
            System.out.println("Exception in eval_string: " + message);
            PlV.sset("main::@", new PlString(message));
        }
        return PlCx.UNDEF;
    }

    static Class<?> compileClassInMemory(String className, String classSourceCode) throws Exception
    {
        SourceCode sourceCodeObj = new SourceCode(className, classSourceCode);
        System.out.println("PlJavaCompiler.compileClassInMemory: name=" + className);
        classLoader.customCompiledCode.put(className, new CompiledCode(className));
        if (fileManager == null) {
            // initializing the file manager
            compilationUnits.add(sourceCodeObj);
            fileManager = new ExtendedStandardJavaFileManager(
                    javac.getStandardFileManager(null, null, null), classLoader);
        }
        else {
            // reusing the file manager; replace the source code
            compilationUnits.set(0, sourceCodeObj);
        }

        List<String> optionList = new ArrayList<String>();
        // set compiler's classpath to be same as the runtime's
        optionList.addAll(Arrays.asList("-classpath", System.getProperty("java.class.path")));

        // run the compiler
        JavaCompiler.CompilationTask task = javac.getTask(null, fileManager,
                null, optionList, null, compilationUnits);
        boolean result = task.call();
        if (!result)
            throw new RuntimeException("Unknown error during compilation.");
        return classLoader.loadClass(className);
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
        System.out.println("ExtendedStandardJavaFileManager.getJavaFileForOutput: name=" + className);
        CompiledCode cc = cl.customCompiledCode.get(className);
        if (cc != null) {
            return cc;
        }
        // source file not found for this output class: this is ok, because we can have a class like 'PlEval$1'
        System.out.println("ExtendedStandardJavaFileManager.getJavaFileForOutput: create name=" + className);
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

    public CompiledCode(String className) throws Exception {
        super(new URI(className), Kind.CLASS);
        this.className = className;
    }
    
    public String getClassName() {
        System.out.println("CompiledCode.getClassName: name=" + className);
        return className;
    }

    @Override
    public OutputStream openOutputStream() throws IOException {
        System.out.println("CompiledCode.openOutputStream()");
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
        System.out.println("DynamicClassLoader.addCode: name=" + cc.getName());
        customCompiledCode.put(cc.getName(), cc);
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        System.out.println("DynamicClassLoader.findClass: name=" + name);
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

EOT

} # end of emit_java()

1;



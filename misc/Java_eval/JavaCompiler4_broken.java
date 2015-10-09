// https://github.com/trung/InMemoryJavaCompiler
//             <name>Apache License, Version 2.0</name>
//              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
// https://github.com/turpid-monkey/InMemoryJavaCompiler
//
import org.mdkt.compiler.InMemoryJavaCompiler;
import org.mdkt.compiler.ExtendedStandardJavaFileManager;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Map;



public class JavaCompiler4_broken
{
    public static void main(String[] args) throws Exception
    {


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
        InMemoryJavaCompiler compiler3 = new InMemoryJavaCompiler();
        compiler3.addSource("org.perlito5.PlInterface", cls4);
        compiler3.addSource("org.perlito5.Adder", cls3);
        Map<String,Class<?>> compiled3 = compiler3.compileAll();
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
            // "public class A{ public B b() { return new B(); }}";
        // String cls2 = "public class B{ public String toString() { return \"B!\"; }}";
        
        InMemoryJavaCompiler compiler = new InMemoryJavaCompiler();
        // compiler.addSource("PlInterface", cls2);
        compiler.addSource("org.perlito5.PlInterface", cls4);
        compiler.addSource("org.perlito5.HelloClass", cls1);
        Map<String,Class<?>> compiled = compiler.compileAll();
        
        Class<?> helloClass = compiled.get("org.perlito5.HelloClass");

        // System.out.println("Loaded class name: " + helloClass.getName());

        // Getting the target method from the loaded class and invoke it using its name
        Method method = helloClass.getMethod("hello", new Class[]{Object.class});
        // System.out.println("Invoked method name: " + method.getName());

        // Adder aaa = new Adder();

        method.invoke(null, aaa);
    }
}

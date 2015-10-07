// https://github.com/trung/InMemoryJavaCompiler
//             <name>Apache License, Version 2.0</name>
//              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
// https://github.com/turpid-monkey/InMemoryJavaCompiler
//
import org.mdkt.compiler.InMemoryJavaCompiler;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Map;

import org.perlito5.PlInterface;

class Adder implements PlInterface {
    public int add(int x, int y) {
        return x + y;
    }
}

public class JavaCompiler3
{
    public static void main(String[] args) throws Exception
    {

        StringBuffer sourceCode = new StringBuffer();
        // sourceCode.append("package org.mdkt;\n");
        sourceCode.append("import org.perlito5.PlInterface;\n");
        sourceCode.append("import java.lang.reflect.Method;\n");
        sourceCode.append("public class HelloClass {\n");
        sourceCode.append("   public static void hello(PlInterface x) { System.out.print(\"hello \" + x.add(3,4) + \"\\n\"); }");
        sourceCode.append("}");


        String cls1 = sourceCode.toString();
            // "public class A{ public B b() { return new B(); }}";
        // String cls2 = "public class B{ public String toString() { return \"B!\"; }}";
        
        InMemoryJavaCompiler compiler = new InMemoryJavaCompiler();
        compiler.addSource("HelloClass", cls1);
        // compiler.addSource("B", cls2);
        Map<String,Class<?>> compiled = compiler.compileAll();
        
        Class<?> helloClass = compiled.get("HelloClass");

        // Class<?> helloClass = InMemoryJavaCompiler.compile("org.mdkt.HelloClass", sourceCode.toString());

        // System.out.println("Loaded class name: " + helloClass.getName());

        // Getting the target method from the loaded class and invoke it using its name
        Method method = helloClass.getMethod("hello", new Class[]{PlInterface.class});
        // System.out.println("Invoked method name: " + method.getName());

        Adder aaa = new Adder();

        method.invoke(null, aaa);
    }
}

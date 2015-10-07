// https://github.com/trung/InMemoryJavaCompiler
//             <name>Apache License, Version 2.0</name>
//              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
import org.mdkt.compiler.InMemoryJavaCompiler;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import org.perlito5.PlInterface;

class Adder implements PlInterface {
    public int add(int x, int y) {
        return x + y;
    }
}

public class JavaCompiler
{
    public static void main(String[] args) throws Exception
    {

        StringBuffer sourceCode = new StringBuffer();
        sourceCode.append("package org.mdkt;\n");
        sourceCode.append("import org.perlito5.PlInterface;\n");
        sourceCode.append("public class HelloClass {\n");
        sourceCode.append("   public static void hello(PlInterface x) { System.out.print(\"hello \" + x.add(3,4) + \"\\n\"); }");
        sourceCode.append("}");

        Class<?> helloClass = InMemoryJavaCompiler.compile("org.mdkt.HelloClass", sourceCode.toString());

        System.out.println("Loaded class name: " + helloClass.getName());

        // Getting the target method from the loaded class and invoke it using its name
        Method method = helloClass.getMethod("hello", new Class[]{PlInterface.class});
        System.out.println("Invoked method name: " + method.getName());

        Adder aaa = new Adder();

        method.invoke(null, aaa);
    }
}

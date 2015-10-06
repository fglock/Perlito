// https://github.com/trung/InMemoryJavaCompiler
//             <name>Apache License, Version 2.0</name>
//              <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
import org.mdkt.compiler.InMemoryJavaCompiler;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

public class JavaCompiler
{
    public static void main(String[] args) throws Exception
    {

        StringBuffer sourceCode = new StringBuffer();
        sourceCode.append("package org.mdkt;\n");
        sourceCode.append("public class HelloClass {\n");
        sourceCode.append("   public static void hello() { System.out.print(\"hello 2\\n\"); }");
        sourceCode.append("}");

        Class<?> helloClass = InMemoryJavaCompiler.compile("org.mdkt.HelloClass", sourceCode.toString());

        System.out.println("Loaded class name: " + helloClass.getName());

        // Create a new instance from the loaded class
        // Constructor constructor = helloClass.getConstructor();
        // Object myClassObject = constructor.newInstance();

        // Getting the target method from the loaded class and invoke it using its name
        Method method = helloClass.getMethod("hello");
        System.out.println("Invoked method name: " + method.getName());
        // method.invoke(myClassObject);
        method.invoke(null);


    }
}

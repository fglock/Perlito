package util.javautil;


import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import java.lang.instrument.Instrumentation;
import java.lang.reflect.Method;

import java.net.URI;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;


public class InFlightCompilation
{
  private static JavaCompiler                        compiler = null;
  private static DiagnosticCollector<JavaFileObject> diagnostics = null;
  private static CustomClassLoader                   customClassLoader = null;

  public static Class compileFromStringAndLoad(String packageName,
                                               String className,
                                               String code) throws Exception
  {
    return compileFromStringAndLoad(packageName, className, null, code);
  }

  public static Class compileFromStringAndLoad(String packageName,
                                               String className,
                                               String classDir,
                                               String code) throws Exception
  {
    return compileFromStringAndLoad(packageName,
                                    className,
                                    classDir,
                                    code,
                                    null,
                                    true);
  }

  public static Class compileFromStringAndLoad(String packageName,
                                               String className,
                                               String code,
                                               ClassLoader cl,
                                               boolean deleteClassWhenLoaded) throws Exception
  {
    return compileFromStringAndLoad(packageName, className, null, code, cl, deleteClassWhenLoaded);
  }
  
  public static Class compileFromStringAndLoad(String packageName,
                                               String className,
                                               String classDir,
                                               String code,
                                               ClassLoader cl,
                                               boolean deleteClassWhenLoaded) throws Exception
  {
    Class loaded = null;

    if (compiler == null)
      compiler = ToolProvider.getSystemJavaCompiler();
    if (diagnostics == null)
      diagnostics = new DiagnosticCollector<JavaFileObject>();

//  JavaFileManager jfm = compiler.getStandardFileManager(null, null, null);
    
    // "Files" to compiles
    ArrayList<JavaFileObject> sfs = new ArrayList<JavaFileObject>();
    sfs.add(new JavaSourceFromString(className, code));
    Iterable<? extends JavaFileObject> compilationUnits = sfs;

    // Call compiler task
    ArrayList<String> options = new ArrayList<String>();
    if (classDir != null)
    {
      options.add("-d");
      options.add(classDir);
      File clsDir = new File(classDir);
      if (!clsDir.exists())
        clsDir.mkdirs();
    }
//  options.add("-cp");
//  options.add(clsDir.toString());

    System.out.print("Compiling..."); 
    JavaMemFileManager jmfm = null;
    if (classDir == null)
     jmfm = new JavaMemFileManager(); // Compiled class is not written on the file system, remains in memory.
    
    if (customClassLoader != null && jmfm != null)
      jmfm.replaceClassLoader(customClassLoader); // Does not work, no impact...

    if (false)
    {
      ClassLoader clldr = jmfm.getClassLoader(StandardLocation.CLASS_PATH);    
      if (clldr != null)
      {
        String clsName = "test.OlivTest";
        try 
        {
          clldr.loadClass(clsName);
          System.err.println(clsName + ":CLASS LOADED");
        } 
        catch (ClassNotFoundException cnfe) 
        {
          System.err.println(clsName + ":NOT LOADED");
        }   
      }
      else
        System.out.println("CLassLoader is null :(");
    }
    
    JavaCompiler.CompilationTask task = compiler.getTask(null, jmfm, diagnostics, options, null, compilationUnits);
    Boolean success = task.call();
    System.out.println("...finished.");

    for (Diagnostic diagnostic: diagnostics.getDiagnostics())
    {
      System.err.println("Code    :" + diagnostic.getCode());
      System.err.println("Kind    :" + diagnostic.getKind());
      System.err.println("Position:" + diagnostic.getPosition());
      System.err.println("Start   :" + diagnostic.getStartPosition());
      System.err.println("End     :" + diagnostic.getEndPosition());
      System.err.println("Source  :" + diagnostic.getSource());
      System.err.println("Message :" + diagnostic.getMessage(null));
      System.err.println("=======================================");
    }

    if (!success.booleanValue())
    {
      String compileError = "";
      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      PrintStream ps = new PrintStream(baos);
      for (Diagnostic diagnostic : diagnostics.getDiagnostics())
      {
        ps.format("Error on line %d in:\n[%s]\n", diagnostic.getLineNumber(), diagnostic.getMessage(Locale.ENGLISH));
        compileError += baos.toString();
        baos.reset();
      }
      throw new RuntimeException(compileError);
    }
    else
    {
      if (customClassLoader == null)
      {
        customClassLoader = new CustomClassLoader(cl != null? cl : ClassLoader.getSystemClassLoader());
//      customClassLoader = new CustomClassLoader(cl != null? cl : Thread.currentThread().getContextClassLoader());        
      }
  
      File classFile = null;
      if (classDir != null)
        classFile = new File(classDir + File.separator + packageName.replaceAll("\\.", File.separator) + File.separator + className + ".class");
      if (classDir != null && classFile.exists())
      {
        try
        {
          long length = classFile.length();
          InputStream cis = new FileInputStream(classFile);
          if (length > Integer.MAX_VALUE)
            throw new RuntimeException("Class file too large...");
          else
          {
            // Read bytes from the class
            byte[] bytes = new byte[(int)length];
            int offset = 0, numread = 0;
            while (offset < bytes.length && (numread = cis.read(bytes, offset, bytes.length-offset)) >= 0)
              offset += numread;
            cis.close();
            if (deleteClassWhenLoaded)
              classFile.delete();
            loaded = customClassLoader.getClassFromBytes(packageName + "." + className, bytes);
          }
        }
        catch (Exception ex)
        {
          throw ex;
        }
      }
      else
      {
        byte[] bytes = jmfm.getClassBytes(packageName + "." + className);
        loaded = customClassLoader.getClassFromBytes(packageName + "." + className, bytes);
        
     // throw new RuntimeException("Class not found ?...");
      }
    }
    return loaded;
  }

  private static final String SAMPLE_CODE =
    "package test;\n\n" +
    "public class OlivTest {\n" +
    "   public static void main(String args[]) {\n" +
    "      System.out.println(\"Output of program:\");\n" +
    "      for (int i=0; i<10; i++) {\n" +
    "         System.out.print(i + \" \");\n" +
    "      }\n" +
    "      System.out.println();\n" +
    "      System.out.println(\"End of output.\");\n" +
    "   }\n" +
    "\n" +
    "    public void sayHi(String name)\n" +
    "    {\n" +
    "      System.out.println(\"Hi \" + name);\n" +
    "    }\n" +
    "}";

  public static void main(String[] args) throws Exception
  {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintStream stdout = System.out;

    System.out.println("Compiling and running new class test.OlivTest");

    ClassLoader classLoader = InFlightCompilation.class.getClassLoader();
    System.out.println("ClassLoader is a " + classLoader.getClass().getName());

    System.setOut(new PrintStream(baos)); // Catch all output
    Class newClass = compileFromStringAndLoad("test", 
                                              "OlivTest", 
                                          //  "generated" + File.separator + "classes", 
                                              SAMPLE_CODE, 
                                              null, 
                                              true);    
    Object obj = newClass.newInstance();
//  System.out.println("Object is a " + obj.getClass().getName());
    Method m = newClass.getMethod("main", String[].class);
    m.invoke(null, new Object[] { null }); // Static, main

    m = newClass.getMethod("sayHi", String.class);
    m.invoke(obj, "Oliv"); // Non Static
    System.setOut(stdout);

    System.out.println("Program Output:");
    System.out.println(baos.toString());
    baos.reset();
    
    Instrumentation instr = JavaAgent.getInstrumentation();
    Class[] alc = instr.getAllLoadedClasses();
    for (int i=0; i<alc.length; i++)
    {
      if (alc[i].getName().startsWith("test"))
        System.out.println("Class [" + alc[i].getName() + "] is loaded");
    }
    
    if (customClassLoader != null)
    {
      System.out.println("CustomClassLoader is not null, good.");
    }
    else
      System.out.println("CustomClassLoader is null");

    if (false)
    {
      try
      {
        System.out.println("Trying to load class test.Test");
     /* Class cls = */ Class.forName("test.Test", true, customClassLoader);
  // /* Class cls = */ Class.forName("test.Test", true, getMyClassLoader());
  // /* Class cls = */ Class.forName("test.Test");
        System.out.println("==> Loaded!..");
      }
      catch (Exception ex)
      {
        System.out.println("==> " + ex.toString());
      }
    }
    /**
     * !!! WARNING, ARGH !!!
     * There is apparently a limitation here.
     * The dynamically compiled and loaded class is NOT part of the classpath of the compiler
     * used by compileFromStringAndLoad.
     * As a result, the compilation below does not work...
     * 
     * The question would be to get the compiler's classpath from the classloader,
     * and not only from the -d option.
     * 
     * See http://bugs.sun.com/view_bug.do?bug_id=6548428
     * 
     * [...] 
     * The crux of the matter is that it is impossible to use JavaCompiler under a custom 
     * classloader (as required by Ant, Webstart and others). 
     * Please let us know if there is a workaround for this issue.
     * 
     * As a result, the execution of the code below fails with a ClassNotFoundException.
     * 
     */ 
    // Override
    String newCode =
      "package test;\n" +
      "\n" +
      "public class extendedTest extends " + newClass.getName() + "\n" +
  //  "public class extendedTest extends test.MyClass\n" +
      "{\n" +
      "  public void sayHi(String name)\n" +
      "  {\n" +
      "    System.out.println(\"Hello Mr \" + name);\n" +
      "  }\n" +
      "  public String greetings(String name)\n" +
      "  {\n" +
      "    return(\"Hello Mr \" + name);\n" +
      "  }\n" +
      "}";
    System.out.println("Compiling:\n" + newCode);

    System.setOut(new PrintStream(baos));
//  Class extendedClass = compileFromStringAndLoad("test", "extendedTest", "generated" + File.separator + "classes", newCode, null, true);
    Class extendedClass = compileFromStringAndLoad("test", 
                                                   "extendedTest", 
                                                   "generated" + File.separator + "classes", 
                                                   newCode, 
                                                   null, // getMyClassLoader(), 
                                                   true);
    obj = extendedClass.newInstance();
    m = extendedClass.getMethod("sayHi", String.class);
    m.invoke(obj, "Oliv"); // Non Static

    System.setOut(stdout);
    System.out.println("Program Output:\n");
    System.out.println(baos.toString());
    baos.reset();
    System.setOut(new PrintStream(baos));

    m = extendedClass.getMethod("greetings", String.class);
    Object greeting = m.invoke(obj, "Oliv"); // Non Static
    System.setOut(stdout);
    System.out.println("Returned:" + greeting.toString());
  }

  public static class JavaSourceFromString extends SimpleJavaFileObject
  {
    /**
     * The source code of this "file".
     */
    final String code;

    /**
     * Constructs a new JavaSourceFromString.
     * @param name the name of the compilation unit represented by this file object
     * @param code the source code for the compilation unit represented by this file object
     */
    JavaSourceFromString(String name, String code)
    {
      super(URI.create("string:///" + name.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
      this.code = code;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors)
    {
      return code;
    }
  }

  public static class ClassMemFileObject extends SimpleJavaFileObject 
  {
      ByteArrayOutputStream os = new ByteArrayOutputStream();

      ClassMemFileObject(String className) 
      {
         super(URI.create("mem:///" + className + Kind.CLASS.extension), Kind.CLASS);
      }
      
      byte[] getBytes() 
      {
         return os.toByteArray();
      }

      @Override
      public OutputStream openOutputStream() throws IOException 
      {
         return os;
      }
   }
  
  public static class JavaMemFileManager extends ForwardingJavaFileManager 
  {
     private HashMap<String, ClassMemFileObject> classes = new HashMap<String, ClassMemFileObject>();
     private ClassLoader customClsLoader = null;
     
     
     public JavaMemFileManager() 
     {
        super(ToolProvider.getSystemJavaCompiler().getStandardFileManager(null, null, null));
     }

     public void replaceClassLoader(ClassLoader cl)
     {
       this.customClsLoader = cl;  
     }
     
     @Override
     public JavaFileObject getJavaFileForOutput(Location location, 
                                                String className, 
                                                JavaFileObject.Kind kind, 
                                                FileObject sibling) 
       throws IOException 
     {
        if (StandardLocation.CLASS_OUTPUT == location && JavaFileObject.Kind.CLASS == kind) 
        {
          ClassMemFileObject clss = new ClassMemFileObject(className);
          classes.put(className, clss);
          return clss;
        } 
        else 
        {
          System.out.println("==> super.getJavaFileForOutput invoked for " + className);
          return super.getJavaFileForOutput(location, className, kind, sibling);
        }
     }

     public byte[] getClassBytes(String className) 
     {
        if (classes.containsKey(className)) 
        {
           return classes.get(className).getBytes();
        }
        return null;
     }

    @Override
    public ClassLoader getClassLoader(JavaFileManager.Location location)
    {
      ClassLoader cl = customClsLoader;
      if (cl == null)
        cl = super.getClassLoader(location);
      return cl;
    }
  }

  public static class CustomClassLoader extends ClassLoader
  {
    public CustomClassLoader()
    {
      super();
    }
    
    public CustomClassLoader(ClassLoader parent)
    {
      super(parent);
    }
    
    public Class getClassFromBytes(String name, byte[] b)
    {
      return defineClass(name, b, 0, b.length);
    }
  }
}
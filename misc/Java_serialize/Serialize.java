// $ cp misc/Java/Serialize.java .
// $ javac -cp .:perlito5.jar Serialize.java
// $ java -cp .:perlito5.jar Serialize

import javax.script.*;
import java.util.*;
import java.io.*;

public class Serialize {
    public static void main(String[] args) throws Exception {

        ScriptEngineManager factory = new ScriptEngineManager();
        ScriptEngine engine = factory.getEngineByName("Perl5");

        // org.perlito.Perlito5.PlObject o = (org.perlito.Perlito5.PlObject) engine.eval("  [1,2,3] ");
        // org.perlito.Perlito5.PlObject o = (org.perlito.Perlito5.PlObject) engine.eval("  'abc' ");

        // TODO
        // org.perlito.Perlito5.PlObject o = (org.perlito.Perlito5.PlObject) engine.eval("  sub { 123 } ");

        org.perlito.Perlito5.PlObject o = (org.perlito.Perlito5.PlObject) engine.eval("  123 ");

        System.out.println("result: " + o);

        System.out.println( Arrays.toString( pickle(o) ) );
        System.out.println( unpickle( pickle(o), org.perlito.Perlito5.PlObject.class ) );
    }


    private static <T extends Serializable> byte[] pickle(T obj) 
           throws IOException 
    {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(obj);
        oos.close();
        return baos.toByteArray();
    }
    
    private static <T extends Serializable> T unpickle(byte[] b, Class<T> cl)
           throws IOException, ClassNotFoundException 
    {
        ByteArrayInputStream bais = new ByteArrayInputStream(b);
        ObjectInputStream ois = new ObjectInputStream(bais);
        Object o = ois.readObject();
        return cl.cast(o);
    }

}

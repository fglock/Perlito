// $ cp misc/Java/Script.java .
// $ javac -cp .:perlito5.jar Script.java 
// $ java -cp .:perlito5.jar Script

import javax.script.*;

public class Script {
    public static void main(String[] args) throws Exception {
    
        ScriptEngineManager factory = new ScriptEngineManager();
        ScriptEngine engine = factory.getEngineByName("Perl5");
    
        Object o = engine.eval(" $x = 456; say 123 + $x; \"value was $x\" ");
        System.out.println("result: " + o);
    }
}


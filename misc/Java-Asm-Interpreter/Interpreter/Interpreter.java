import java.util.List;

public class Interpreter {
    public static void main(String[] args) throws Exception {

        String[] code = {
            "2",
            "2 + 5",
            "print 2 + 5;",
            "sub add (value) { return value + 2; }",
            "add(5)"
        };

        for (String s: code) {
            System.out.println("Input: [[[ " + s + " ]]]");
            Lexer lexer1 = new Lexer(s);
            Parser parser1 = new Parser(lexer1.tokenize());
            List<Node> statements1 = parser1.parse();
            JITCompiler.compileAndRun(statements1);
        }

        // Lexer lexer2 = new Lexer(code2);
        // Parser parser2 = new Parser(lexer2.tokenize());
        // List<Node> statements2 = parser2.parse();

        // JITCompiler.compileAndRun(statements1);
        // int result = JITCompiler.compileAndRun(statements2);

        // System.out.println("Result: " + result);
    }
}

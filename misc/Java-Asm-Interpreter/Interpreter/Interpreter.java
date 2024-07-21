import java.util.List;

public class Interpreter {
    public static void main(String[] args) throws Exception {
        String code1 = "sub add(value) { return value + 2; }";
        String code2 = "print add(5);";

        Lexer lexer1 = new Lexer(code1);
        List<Token> tokens1 = lexer1.tokenize();
        Parser parser1 = new Parser(tokens1);
        List<Node> statements1 = parser1.parse();

        Lexer lexer2 = new Lexer(code2);
        List<Token> tokens2 = lexer2.tokenize();
        Parser parser2 = new Parser(tokens2);
        List<Node> statements2 = parser2.parse();

        JITCompiler.compileAndRun(statements1);
        JITCompiler.compileAndRun(statements2);
    }
}

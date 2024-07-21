public class Interpreter {
    public static void main(String[] args) {
        String code1 = "sub add (value) { return value + 2; }";
        String code2 = "add(5)";

        Parser parser1 = new Parser(code1);
        JITCompiler.compileAndRun(parser1);

        Parser parser2 = new Parser(code2);
        JITCompiler.compileAndRun(parser2);
    }
}


public class Interpreter {
    public static void main(String[] args) {
        String code1 = "sub add (value) { return value + 2; }";
        String code2 = "print add(5);";

        Parser parser = new Parser(code1);
        JITCompiler.compileAndRun(parser);

        parser = new Parser(code2);
        JITCompiler.compileAndRun(parser);
    }
}


public class Interpreter {
    public static void main(String[] args) {
        try {
            // Example code string to be parsed and compiled
            // String code = "sub add { return 1 + 2; } add();"; // Replace with actual code to execute
            // String code = "sub add { return + 1 2; } add();"; // Replace with actual code to execute
            // String code = "sub add { return 1 + 2 } add();"; // Replace with actual code to execute
            String code = "sub add { return 1 + 2; } add();"; // Replace with actual code to execute
            
            // Create a parser instance with the code
            Parser parser = new Parser(code);
            
            // Parse the code using the parser
            Node parsedCode = parser.parse();
            
            // Compile and run the parsed code
            int result = JITCompiler.compileAndRun(parsedCode);
            System.out.println("Result: " + result);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

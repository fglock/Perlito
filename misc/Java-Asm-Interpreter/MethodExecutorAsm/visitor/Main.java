import java.util.List;

public class Main {
    public static void main(String[] args) {
        // Create an example AST
        Node ast = new BlockNode(List.of(
            new VariableNode("a"),
            new BinaryOpNode(
                new VariableNode("a"),
                "+",
                new NumberNode("42")
            )
        ));

        // Generate code
        CodeGenerator generator = new CodeGenerator();
        ast.accept(generator);

        // Print generated code
        System.out.println(generator.getCode());
    }
}

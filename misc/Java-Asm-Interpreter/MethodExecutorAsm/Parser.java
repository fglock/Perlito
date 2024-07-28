import java.util.ArrayList;
import java.util.List;

class Parser {
    public final Lexer lexer;
    public Token currentToken;

    public Parser(Lexer lexer) {
        this.lexer = lexer;
        this.currentToken = lexer.nextToken();
    }

    public Node parse() {
        List<Node> statements = new ArrayList<>();
        while (currentToken.type != TokenType.EOF) {
            statements.add(parseStatement());
        }
        return new BlockNode(statements);
    }

    public Node parseStatement() {
        Node expr = parseExpression();
        if (currentToken.type == TokenType.NEWLINE) {
            advance();
        }
        return expr;
    }

    public Node parseExpression() {
        Node left = parsePrimary();
        while (currentToken.type == TokenType.OPERATOR) {
            String operator = currentToken.text;
            advance();
            Node right = parsePrimary();
            left = new BinaryOpNode(left, operator, right);
        }
        return left;
    }

    public Node parsePrimary() {
        Token token = currentToken;
        switch (token.type) {
            case IDENTIFIER:
                advance();
                return new VariableNode(token.text);
            case NUMBER:
                advance();
                return new NumberNode(token.text);
            case STRING:
                advance();
                return new StringNode(token.text);
            default:
                throw new RuntimeException("Unexpected token: " + token);
        }
    }

    public void advance() {
        currentToken = lexer.nextToken();
    }

        public static void main(String[] args) {
            String code = "$var1 = 42 + 3.14 * 2";
            Lexer lexer = new Lexer(code);
            Parser parser = new Parser(lexer);
            Node ast = parser.parse();
            
            // Print the AST or process it further
            System.out.println(ast);
        }

}

// AST Node classes
abstract class Node {}

class BlockNode extends Node {
    List<Node> statements;

    BlockNode(List<Node> statements) {
        this.statements = statements;
    }
}

class VariableNode extends Node {
    String name;

    VariableNode(String name) {
        this.name = name;
    }
}

class NumberNode extends Node {
    String value;

    NumberNode(String value) {
        this.value = value;
    }
}

class StringNode extends Node {
    String value;

    StringNode(String value) {
        this.value = value;
    }
}

class BinaryOpNode extends Node {
    Node left;
    String operator;
    Node right;

    BinaryOpNode(Node left, String operator, Node right) {
        this.left = left;
        this.operator = operator;
        this.right = right;
    }
}

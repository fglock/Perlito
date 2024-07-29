import java.util.*;

public class Parser {
    private final List<Token> tokens;
    private int position = 0;

    public Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    public Node parse() {
        return parseExpression(0);
    }

    private Node parseExpression(int precedence) {
        Node left = parsePrimary();

        while (true) {
            Token token = peek();
            if (token.type == TokenType.EOF || token.text.equals(":")) {
                break;
            }

            int tokenPrecedence = getPrecedence(token);

            if (tokenPrecedence < precedence) {
                break;
            }

            if (isRightAssociative(token)) {
                left = parseInfix(left, tokenPrecedence - 1);
            } else {
                left = parseInfix(left, tokenPrecedence);
            }
        }

        return left;
    }

    private Node parsePrimary() {
        Token token = consume();

        switch (token.type) {
            case IDENTIFIER:
                return new IdentifierNode(token.text);
            case NUMBER:
                return new NumberNode(token.text);
            case STRING:
                return new StringNode(token.text);
            case OPERATOR:
                if (token.text.equals("(")) {
                    Node expr = parseExpression(0);
                    consume(TokenType.OPERATOR, ")");
                    return expr;
                } else if (token.text.equals("$")) {
                    Node operand = parsePrimary();
                    return new UnaryOperatorNode("$", operand);
                }
                break;
            case EOF:
                return null; // End of input
            default:
                throw new RuntimeException("Unexpected token: " + token);
        }
        throw new RuntimeException("Unexpected token: " + token);
    }

    private Node parseInfix(Node left, int precedence) {
        Token token = consume();

        if (isTernaryOperator(token)) {
            Node middle = parseExpression(0);
            consume(TokenType.OPERATOR, ":");
            Node right = parseExpression(precedence);
            return new TernaryOperatorNode(token.text, left, middle, right);
        }

        Node right = parseExpression(precedence);

        switch (token.text) {
            case "+":
            case "-":
            case "*":
            case "/":
            case "=":
                return new BinaryOperatorNode(token.text, left, right);
            case "->":
                return new PostfixOperatorNode(token.text, left);
            // Handle other infix operators
            default:
                throw new RuntimeException("Unexpected infix operator: " + token);
        }
    }

    private Token peek() {
        while (position < tokens.size() && (tokens.get(position).type == TokenType.WHITESPACE || tokens.get(position).type == TokenType.NEWLINE)) {
            position++;
        }
        if (position >= tokens.size()) {
            return new Token(TokenType.EOF, "");
        }
        return tokens.get(position);
    }

    private Token consume() {
        while (position < tokens.size() && (tokens.get(position).type == TokenType.WHITESPACE || tokens.get(position).type == TokenType.NEWLINE)) {
            position++;
        }
        if (position >= tokens.size()) {
            return new Token(TokenType.EOF, "");
        }
        return tokens.get(position++);
    }

    private void consume(TokenType type, String text) {
        Token token = consume();
        if (token.type != type || !token.text.equals(text)) {
            throw new RuntimeException("Expected token " + type + " with text " + text + " but got " + token);
        }
    }

    private int getPrecedence(Token token) {
        // Define precedence levels for operators
        switch (token.text) {
            case "=":
                return 1; // Lower precedence for assignment
            case "?":
                return 2; // Precedence for ternary operator
            case "+":
            case "-":
                return 3;
            case "*":
            case "/":
                return 4;
            case "->":
                return 5;
            case "$":
                return 6; // Higher precedence for prefix operator
            default:
                return 0;
        }
    }

    private boolean isRightAssociative(Token token) {
        // Define right associative operators
        switch (token.text) {
            case "=":
            case "?":
                return true;
            default:
                return false;
        }
    }

    private boolean isTernaryOperator(Token token) {
        return token.text.equals("?");
    }

    // Abstract syntax tree (AST) node classes
    abstract class Node {}

    class IdentifierNode extends Node {
        String name;

        IdentifierNode(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return "Identifier(" + name + ")";
        }
    }

    class NumberNode extends Node {
        String value;

        NumberNode(String value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "Number(" + value + ")";
        }
    }

    class StringNode extends Node {
        String value;

        StringNode(String value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "String(" + value + ")";
        }
    }

    class BinaryOperatorNode extends Node {
        String operator;
        Node left;
        Node right;

        BinaryOperatorNode(String operator, Node left, Node right) {
            this.operator = operator;
            this.left = left;
            this.right = right;
        }

        @Override
        public String toString() {
            return "BinaryOperator(" + operator + ", " + left + ", " + right + ")";
        }
    }

    class PostfixOperatorNode extends Node {
        String operator;
        Node operand;

        PostfixOperatorNode(String operator, Node operand) {
            this.operator = operator;
            this.operand = operand;
        }

        @Override
        public String toString() {
            return "PostfixOperator(" + operator + ", " + operand + ")";
        }
    }

    class TernaryOperatorNode extends Node {
        String operator;
        Node condition;
        Node trueExpr;
        Node falseExpr;

        TernaryOperatorNode(String operator, Node condition, Node trueExpr, Node falseExpr) {
            this.operator = operator;
            this.condition = condition;
            this.trueExpr = trueExpr;
            this.falseExpr = falseExpr;
        }

        @Override
        public String toString() {
            return "TernaryOperator(" + condition + " ? " + trueExpr + " : " + falseExpr + ")";
        }
    }

    class UnaryOperatorNode extends Node {
        String operator;
        Node operand;

        UnaryOperatorNode(String operator, Node operand) {
            this.operator = operator;
            this.operand = operand;
        }

        @Override
        public String toString() {
            return "UnaryOperator(" + operator + ", " + operand + ")";
        }
    }

    class ListNode extends Node {
        List<Node> elements;

        ListNode(List<Node> elements) {
            this.elements = elements;
        }

        @Override
        public String toString() {
            return "List(" + elements + ")";
        }
    }

    // Example of a method to parse a list
    private Node parseList() {
        List<Node> elements = new ArrayList<>();
        consume(TokenType.OPERATOR, "(");
        while (!peek().text.equals(")")) {
            elements.add(parseExpression(0));
            if (peek().text.equals(",")) {
                consume();
            }
        }
        consume(TokenType.OPERATOR, ")");
        return new ListNode(elements);
    }

    public static void main(String[] args) {
        String code = "my $var = 42; 1 ? 2 : 3; print \"Hello, World!\\n\";";
        Lexer lexer = new Lexer(code);
        List<Token> tokens = lexer.tokenize();
        Parser parser = new Parser(tokens);
        Node ast = parser.parse();
        System.out.println(ast);
    }
}


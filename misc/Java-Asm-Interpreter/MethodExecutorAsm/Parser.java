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
                }
                // Handle other primary cases like lists, etc.
                throw new RuntimeException("Unexpected operator: " + token); // Ensure all paths return a value
            default:
                throw new RuntimeException("Unexpected token: " + token);
        }
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
                return new BinaryOperatorNode(token.text, left, right);
            case "->":
                return new PostfixOperatorNode(token.text, left);
            // Handle other infix operators
            default:
                throw new RuntimeException("Unexpected infix operator: " + token);
        }
    }

    private Token peek() {
        if (position >= tokens.size()) {
            return new Token(TokenType.EOF, "");
        }
        return tokens.get(position);
    }

    private Token consume() {
        Token token = peek();
        position++;
        return token;
    }

    private Token consume(TokenType type, String text) {
        Token token = peek();
        if (token.type != type || !token.text.equals(text)) {
            throw new RuntimeException("Expected token: " + text + " but found: " + token);
        }
        return consume();
    }

    private int getPrecedence(Token token) {
        // Define precedence levels for operators
        switch (token.text) {
            case "+":
            case "-":
                return 10;
            case "*":
            case "/":
                return 20;
            case "->":
                return 30;
            case "?":
                return 5; // Ternary operator precedence
            // Define other precedence levels
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

    class BinaryOperatorNode extends Node {
        String operator;
        Node left;
        Node right;

        BinaryOperatorNode(String operator, Node left, Node right) {
            this.operator = operator;
            this.left = left;
            this.right = right;
        }
    }

    class PostfixOperatorNode extends Node {
        String operator;
        Node operand;

        PostfixOperatorNode(String operator, Node operand) {
            this.operator = operator;
            this.operand = operand;
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
    }

    // Additional node classes for completeness
    class UnaryOperatorNode extends Node {
        String operator;
        Node operand;

        UnaryOperatorNode(String operator, Node operand) {
            this.operator = operator;
            this.operand = operand;
        }
    }

    class ListNode extends Node {
        List<Node> elements;

        ListNode(List<Node> elements) {
            this.elements = elements;
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
}


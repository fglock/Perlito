import java.util.ArrayList;
import java.util.List;

public class Parser {
    private List<Token> tokens;
    private int position;

    public Parser(List<Token> tokens) {
        this.tokens = tokens;
        this.position = 0;
    }

    private Token peek() {
        if (position < tokens.size()) {
            return tokens.get(position);
        }
        return new Token(TokenType.EOF, "");
    }

    private Token consume() {
        if (position < tokens.size()) {
            return tokens.get(position++);
        }
        return new Token(TokenType.EOF, "");
    }

    private boolean match(TokenType type) {
        if (peek().type == type) {
            consume();
            return true;
        }
        return false;
    }

    private void skipWhitespace() {
        while (peek().type == TokenType.WHITESPACE || peek().type == TokenType.NEWLINE) {
            consume();
        }
    }

    public Node parse() {
        skipWhitespace();
        return parseBlock();
    }

    private BlockNode parseBlock() {
        List<Node> statements = new ArrayList<>();
        while (peek().type != TokenType.EOF) {
            statements.add(parseStatement());
            skipWhitespace();
        }
        return new BlockNode(statements);
    }

    private Node parseStatement() {
        skipWhitespace();
        Token token = peek();

        if (token.type == TokenType.IDENTIFIER && token.text.equals("my")) {
            consume();
            skipWhitespace();
            return parseVariableDeclaration();
        } else {
            return parseExpression();
        }
    }

    private Node parseVariableDeclaration() {
        skipWhitespace();
        List<VariableNode> variables = new ArrayList<>();
        while (peek().type == TokenType.OPERATOR && (peek().text.equals("$") || peek().text.equals("@") || peek().text.equals("%"))) {
            Token varTypeToken = consume();
            Token nameToken = consume();
            if (nameToken.type != TokenType.IDENTIFIER) {
                throw new RuntimeException("Expected identifier but found " + nameToken.text);
            }
            variables.add(new VariableNode(varTypeToken.text + nameToken.text));
            skipWhitespace();
            if (peek().type == TokenType.OPERATOR && peek().text.equals(",")) {
                consume();
                skipWhitespace();
            } else {
                break;
            }
        }

        Token eqToken = consume(); // Consume the '=' operator
        if (eqToken.type != TokenType.OPERATOR || !eqToken.text.equals("=")) {
            throw new RuntimeException("Expected '=' but found " + eqToken.text);
        }

        skipWhitespace();
        List<Node> values = parseExpressionList();

        Token semiToken = consume(); // Consume the ';' operator
        if (semiToken.type != TokenType.OPERATOR || !semiToken.text.equals(";")) {
            throw new RuntimeException("Expected ';' but found " + semiToken.text);
        }

        return new VariableDeclarationNode(variables, values);
    }

    private List<Node> parseExpressionList() {
        List<Node> expressions = new ArrayList<>();
        expressions.add(parseExpression());
        while (peek().type == TokenType.OPERATOR && peek().text.equals(",")) {
            consume();
            skipWhitespace();
            expressions.add(parseExpression());
        }
        return expressions;
    }

    private Node parseExpression() {
        Node left = parsePrimary();
        skipWhitespace();
        while (peek().type == TokenType.OPERATOR && (peek().text.equals("+") || peek().text.equals("-") || peek().text.equals("**"))) {
            Token operator = consume();
            skipWhitespace();
            Node right = parsePrimary();
            left = new BinaryOpNode(left, operator.text, right);
            skipWhitespace();
        }
        return left;
    }

    private Node parsePrimary() {
        Token token = consume();
        switch (token.type) {
            case IDENTIFIER:
                return new VariableNode(token.text);
            case NUMBER:
                return new NumberNode(token.text);
            case STRING:
                return new StringNode(token.text);
            case OPERATOR:
                if (token.text.equals("(")) {
                    List<Node> expressions = parseExpressionList();
                    if (peek().type == TokenType.OPERATOR && peek().text.equals(")")) {
                        consume(); // consume ')'
                        return new TupleNode(expressions);
                    } else {
                        throw new RuntimeException("Expected ')' but found " + peek().text);
                    }
                } else if (token.text.equals("$") || token.text.equals("@") || token.text.equals("%")) {
                    Token nameToken = consume();
                    if (nameToken.type != TokenType.IDENTIFIER) {
                        throw new RuntimeException("Expected identifier but found " + nameToken.text);
                    }
                    return new VariableNode(token.text + nameToken.text);
                } else if (token.text.equals("[")) {
                    Node index = parseExpression();
                    if (peek().type == TokenType.OPERATOR && peek().text.equals("]")) {
                        consume(); // consume ']'
                        return new ArrayIndexNode(new VariableNode(token.text), index);
                    } else {
                        throw new RuntimeException("Expected ']' but found " + peek().text);
                    }
                }
            default:
                throw new RuntimeException("Unexpected token: " + token);
        }
    }

    public static void main(String[] args) {
        List<Token> tokens = List.of(
            new Token(TokenType.IDENTIFIER, "my"),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.OPERATOR, "$"),
            new Token(TokenType.IDENTIFIER, "b"),
            new Token(TokenType.OPERATOR, ","),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.OPERATOR, "@"),
            new Token(TokenType.IDENTIFIER, "a"),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.OPERATOR, "="),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.OPERATOR, "("),
            new Token(TokenType.NUMBER, "7"),
            new Token(TokenType.OPERATOR, ","),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.NUMBER, "8"),
            new Token(TokenType.OPERATOR, ","),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.NUMBER, "9"),
            new Token(TokenType.OPERATOR, ")"),
            new Token(TokenType.OPERATOR, ";"),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.OPERATOR, "$"),
            new Token(TokenType.IDENTIFIER, "a"),
            new Token(TokenType.OPERATOR, "["),
            new Token(TokenType.NUMBER, "10"),
            new Token(TokenType.OPERATOR, "]"),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.OPERATOR, "+"),
            new Token(TokenType.WHITESPACE, " "),
            new Token(TokenType.OPERATOR, "$"),
            new Token(TokenType.IDENTIFIER, "b"),
            new Token(TokenType.OPERATOR, "**"),
            new Token(TokenType.NUMBER, "2"),
            new Token(TokenType.EOF, "")
        );

        Parser parser = new Parser(tokens);
        Node ast = parser.parse();
        System.out.println(ast.toString(0));
    }
}

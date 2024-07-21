import java.util.*;

public class Parser {
    private final List<Token> tokens;
    private int position;
    private final Map<String, Integer> localVariables = new HashMap<>();
    private int localVariableIndex = 0;

    public Parser(List<Token> tokens) {
        this.tokens = tokens;
        this.position = 0;
    }

    public List<Node> parse() {
        List<Node> statements = new ArrayList<>();
        while (!isAtEnd()) {
            statements.add(parseStatement());
        }
        return statements;
    }

    private Node parseStatement() {
        if (match(TokenType.RETURN)) {
            return parseReturnStatement();
        } else if (match(TokenType.PRINT)) {
            return parsePrintStatement();
        } else if (match(TokenType.SUB)) {
            return parseSubroutineDeclaration();
        } else {
            return parseExpression();
        }
    }

    private Node parseReturnStatement() {
        Node expression = parseExpression();
        expect(TokenType.SEMICOLON);
        return new ReturnNode(expression);
    }

    private Node parsePrintStatement() {
        Node expression = parseExpression();
        expect(TokenType.SEMICOLON);
        return new PrintNode(expression);
    }

    private Node parseSubroutineDeclaration() {
        expect(TokenType.SUB);
        // String name = parseIdentifier().getName();
        String name = parseIdentifier();
        // String name = ((IdentifierNode) parseIdentifier()).getName();
        expect(TokenType.LEFT_PAREN);

        List<String> parameters = new ArrayList<>();
        if (!match(TokenType.RIGHT_PAREN)) {
            do {
                Token identifierToken = expect(TokenType.IDENTIFIER);
                String paramName = identifierToken.value;
                parameters.add(paramName);
                localVariables.put(paramName, localVariableIndex++);
            } while (match(TokenType.COMMA));
            expect(TokenType.RIGHT_PAREN);
        }

        Node body = parseBlock();
        return new SubroutineDeclarationNode(name, parameters, body);
    }

    private Node parseBlock() {
        List<Node> statements = new ArrayList<>();
        expect(TokenType.LEFT_BRACE);
        while (!match(TokenType.RIGHT_BRACE) && !isAtEnd()) {
            statements.add(parseStatement());
        }
        expect(TokenType.RIGHT_BRACE);
        return new StatementsNode(statements);
    }

    private Node parseExpression() {
        Node left = parsePrimary();
        while (match(TokenType.PLUS)) {
            TokenType op = previous().type;
            Node right = parsePrimary();
            left = new BinaryOpNode(op, left, right);
        }
        return left;
    }

    private Node parsePrimary() {
        if (match(TokenType.NUMBER)) {
            return new NumberNode(Double.parseDouble(previous().value));
        } else if (match(TokenType.IDENTIFIER)) {
            String name = previous().value;
            if (match(TokenType.LEFT_PAREN)) {
                List<Node> arguments = new ArrayList<>();
                if (!match(TokenType.RIGHT_PAREN)) {
                    do {
                        arguments.add(parseExpression());
                    } while (match(TokenType.COMMA));
                    expect(TokenType.RIGHT_PAREN);
                }
                return new SubroutineCallNode(name, arguments);
            }
            int index = localVariables.getOrDefault(name, -1);
            return new IdentifierNode(name, index);
        } else {
            throw new RuntimeException("Unexpected token: " + peek().type);
        }
    }

    private boolean match(TokenType type) {
        if (check(type)) {
            advance();
            return true;
        }
        return false;
    }

    private boolean check(TokenType type) {
        if (isAtEnd()) return false;
        return peek().type == type;
    }

    private Token advance() {
        if (!isAtEnd()) position++;
        return previous();
    }

    private boolean isAtEnd() {
        return position >= tokens.size();
    }

    private Token peek() {
        return tokens.get(position);
    }

    private Token previous() {
        return tokens.get(position - 1);
    }

    private Token expect(TokenType type) {
        if (check(type)) return advance();
        throw new RuntimeException("Expected token: " + type);
    }

    private String parseIdentifier() {
        Token identifierToken = expect(TokenType.IDENTIFIER);
        return identifierToken.value;
    }
}

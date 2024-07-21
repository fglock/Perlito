import java.util.*;

public class Parser {
    private final List<Token> tokens;
    private int pos;

    public Parser(List<Token> tokens) {
        this.tokens = tokens;
        this.pos = 0;
    }

    public List<Node> parse() {
        List<Node> statements = new ArrayList<>();
        while (pos < tokens.size()) {
            statements.add(parseStatement());
        }
        return statements;
    }

    private Node parseSubroutineDeclaration() {
        expect(TokenType.KEYWORD, "sub");
        String name = parseIdentifier().value;
        List<String> parameters = parseParameters();
        Node body = parseBlock();
        return new SubroutineDeclarationNode(name, parameters, body);
    }

    private List<String> parseParameters() {
        List<String> parameters = new ArrayList<>();
        expect(TokenType.LPAREN);
        while (!match(TokenType.RPAREN)) {
            parameters.add(parseIdentifier().value);
            if (!match(TokenType.RPAREN)) {
                expect(TokenType.OPERATOR, ",");
            }
        }
        return parameters;
    }

    private Node parseBlock() {
        expect(TokenType.LBRACE);
        List<Node> statements = new ArrayList<>();
        while (!match(TokenType.RBRACE)) {
            statements.add(parseStatement());
        }
        return new StatementsNode(statements);
    }

    private Node parseStatement() {
        if (match(TokenType.KEYWORD, "print")) {
            return parsePrintStatement();
        } else if (match(TokenType.KEYWORD, "return")) {
            return parseReturnStatement();
        } else if (match(TokenType.KEYWORD, "sub")) {
            return parseSubroutineDeclaration();
        } else {
            Node expr = parseExpression();
            expect(TokenType.SEMICOLON);
            return expr;
        }
    }

    private Node parsePrintStatement() {
        expect(TokenType.KEYWORD, "print");
        Node expression = parseExpression();
        expect(TokenType.SEMICOLON);
        return new PrintNode(expression);
    }

    private Node parseReturnStatement() {
        expect(TokenType.KEYWORD, "return");
        Node expression = parseExpression();
        expect(TokenType.SEMICOLON);
        return new ReturnNode(expression);
    }

    private Node parseExpression() {
        Node left = parsePrimary();
        while (match(TokenType.OPERATOR)) {
            String operator = tokens.get(pos - 1).value;
            Node right = parsePrimary();
            left = new BinaryOpNode(operator.charAt(0), left, right);
        }
        return left;
    }

    private Node parsePrimary() {
        Token token = next();
        switch (token.type) {
            case NUMBER:
                return new NumberNode(Integer.parseInt(token.value));
            case IDENTIFIER:
                return new IdentifierNode(token.value);
            case LPAREN:
                Node expr = parseExpression();
                expect(TokenType.RPAREN);
                return expr;
            default:
                throw new RuntimeException("Unexpected character: " + token.value);
        }
    }

    private Token parseIdentifier() {
        Token token = next();
        if (token.type != TokenType.IDENTIFIER) {
            throw new RuntimeException("Expected identifier, found " + token.value);
        }
        return token;
    }

    private boolean match(TokenType type) {
        if (pos < tokens.size() && tokens.get(pos).type == type) {
            pos++;
            return true;
        }
        return false;
    }

    private boolean match(TokenType type, String value) {
        if (pos < tokens.size() && tokens.get(pos).type == type && tokens.get(pos).value.equals(value)) {
            pos++;
            return true;
        }
        return false;
    }

    private Token expect(TokenType type) {
        if (pos < tokens.size() && tokens.get(pos).type == type) {
            return tokens.get(pos++);
        }
        throw new RuntimeException("Expected " + type + ", found " + tokens.get(pos).value);
    }

    private Token expect(TokenType type, String value) {
        if (pos < tokens.size() && tokens.get(pos).type == type && tokens.get(pos).value.equals(value)) {
            return tokens.get(pos++);
        }
        throw new RuntimeException("Expected " + type + " " + value + ", found " + tokens.get(pos).value);
    }

    private Token next() {
        return tokens.get(pos++);
    }
}

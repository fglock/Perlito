import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class Parser {
    private final String code;
    private int index;

    public Parser(String code) {
        this.code = code;
        this.index = 0;
    }

    public List<Node> parse() {
        List<Node> nodes = new ArrayList<>();
        while (index < code.length()) {
            nodes.add(parseSubroutineDeclaration());
        }
        return nodes;
    }

    private SubroutineDeclarationNode parseSubroutineDeclaration() {
        skipWhitespace();
        expect("sub");

        skipWhitespace();
        String subroutineName = parseIdentifier().getName();

        skipWhitespace();
        expect('(');
        List<String> parameters = new ArrayList<>();
        while (peek() != ')') {
            skipWhitespace();
            parameters.add(parseIdentifier().getName());
            skipWhitespace();
            if (peek() == ',') {
                index++;
            }
        }
        expect(')');

        skipWhitespace();
        expect('{');

        Node body = parseBlock();

        skipWhitespace();
        expect('}');

        return new SubroutineDeclarationNode(subroutineName, parameters, body);
    }

    private StatementsNode parseBlock() {
        List<Node> statements = new ArrayList<>();
        while (peek() != '}') {
            statements.add(parseStatement());
            skipWhitespace();
        }
        return new StatementsNode(statements);
    }

    private Node parseStatement() {
        skipWhitespace();
        if (peek() == 'p' && code.startsWith("print", index)) {
            index += 5;
            skipWhitespace();
            Node expression = parseExpression();
            skipWhitespace();
            expect(';');
            return new PrintNode(expression);
        } else {
            return parseExpression();
        }
    }

    private Node parseExpression() {
        skipWhitespace();
        Node left = parsePrimary();
        skipWhitespace();
        while (peek() == '+' || peek() == '-' || peek() == '*' || peek() == '/' || peek() == '%') {
            char op = next();
            skipWhitespace();
            Node right = parsePrimary();
            left = new BinaryOpNode(op, left, right);
            skipWhitespace();
        }
        return left;
    }

    private Node parsePrimary() {
        skipWhitespace();
        char c = peek();
        if (Character.isDigit(c)) {
            return new NumberNode(parseNumber());
        } else if (Character.isLetter(c)) {
            String name = parseIdentifier().getName();
            skipWhitespace();
            if (peek() == '(') {
                index++;
                skipWhitespace();
                Node argument = parseExpression();
                skipWhitespace();
                expect(')');
                return new SubroutineCallNode(name, argument);
            } else {
                return new IdentifierNode(name, -1); // For simplicity, using -1 for the localIndex
            }
        } else {
            throw new RuntimeException("Unexpected character: " + c);
        }
    }

    private int parseNumber() {
        int start = index;
        while (index < code.length() && Character.isDigit(code.charAt(index))) {
            index++;
        }
        return Integer.parseInt(code.substring(start, index));
    }

    private IdentifierNode parseIdentifier() {
        int start = index;
        while (index < code.length() && Character.isLetterOrDigit(code.charAt(index))) {
            index++;
        }
        return new IdentifierNode(code.substring(start, index), -1); // For simplicity, using -1 for the localIndex
    }

    private void expect(char expected) {
        if (peek() != expected) {
            throw new RuntimeException("Expected '" + expected + "'");
        }
        index++;
    }

    private void expect(String expected) {
        if (!code.startsWith(expected, index)) {
            throw new RuntimeException("Expected '" + expected + "'");
        }
        index += expected.length();
    }

    private void skipWhitespace() {
        while (index < code.length() && Character.isWhitespace(code.charAt(index))) {
            index++;
        }
    }

    private char peek() {
        if (index >= code.length()) {
            return '\0';
        }
        return code.charAt(index);
    }

    private char next() {
        char c = peek();
        index++;
        return c;
    }
}


import java.util.*;

public class Parser {
    private String code;
    private int pos;

    public Parser(String code) {
        this.code = code;
        this.pos = 0;
    }

    public Node parse() {
        skipWhitespace();
        if (code.startsWith("sub ", pos)) {
            return parseSubroutineDeclaration();
        } else {
            return parseExpression();
        }
    }

    private Node parseExpression() {
        Node left = parsePrimary();
        skipWhitespace();
        while (pos < code.length() && "+-*/".indexOf(code.charAt(pos)) != -1) {
            char op = code.charAt(pos++);
            Node right = parsePrimary();
            left = new BinaryOpNode(left, right, Character.toString(op));
            skipWhitespace();
        }
        return left;
    }

    private Node parsePrimary() {
        skipWhitespace();
        if (Character.isDigit(code.charAt(pos))) {
            return parseNumber();
        } else if (Character.isLetter(code.charAt(pos))) {
            String identifier = parseIdentifier().toString();
            if (identifier.equals("return")) {
                return parseReturn();
            } else if (identifier.equals("print")) {
                return parsePrint();
            } else {
                return new IdentifierNode(identifier);
            }
        } else {
            throw new IllegalArgumentException("Unexpected character: " + code.charAt(pos));
        }
    }
    
    private Node parseReturn() {
        skipWhitespace();
        Node expression = parseExpression(); // Parse the return expression
        return new ReturnNode(expression);
    }

    private Node parsePrint() {
        skipWhitespace();
        if (code.charAt(pos) != '(') {
            throw new IllegalArgumentException("Expected '(' after 'print'");
        }
        pos++; // Skip '('
        Node expression = parseExpression();
        skipWhitespace();
        if (code.charAt(pos) != ')') {
            throw new IllegalArgumentException("Expected ')' after print expression");
        }
        pos++; // Skip ')'
        return new PrintNode(expression);
    }

    private Node parseNumber() {
        int start = pos;
        while (pos < code.length() && Character.isDigit(code.charAt(pos))) {
            pos++;
        }
        int value = Integer.parseInt(code.substring(start, pos));
        return new NumberNode(value);
    }

    private Node parseIdentifier() {
        int start = pos;
        while (pos < code.length() && Character.isLetterOrDigit(code.charAt(pos))) {
            pos++;
        }
        String name = code.substring(start, pos);
        return new IdentifierNode(name);
    }

    private Node parseSubroutineDeclaration() {
        pos += 4; // Skip 'sub '
        String name = parseIdentifier().toString();
        skipWhitespace();
        if (code.charAt(pos) != '{') {
            throw new IllegalArgumentException("Expected '{' after subroutine name");
        }
        pos++; // Skip '{'
        Node body = parseStatements();
        if (code.charAt(pos) != '}') {
            throw new IllegalArgumentException("Expected '}' at the end of subroutine");
        }
        pos++; // Skip '}'
        return new SubroutineDeclarationNode(name, body);
    }

    private Node parseStatements() {
        List<Node> statements = new ArrayList<>();
        while (pos < code.length() && code.charAt(pos) != '}') {
            statements.add(parseExpression());
            skipWhitespace();
            if (pos < code.length() && code.charAt(pos) == ';') {
                pos++;
            }
            skipWhitespace();
        }
        return new StatementsNode(statements);
    }

    private void skipWhitespace() {
        while (pos < code.length() && Character.isWhitespace(code.charAt(pos))) {
            pos++;
        }
    }
}


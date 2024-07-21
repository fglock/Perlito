import java.util.*;

public class Parser {
    private String code;
    private int pos = 0;

    public Parser(String code) {
        this.code = code;
    }

    public Node parse() {
        return parseSubroutineDeclaration();
    }

    private Node parseSubroutineDeclaration() {
        skipWhitespace();
        if (code.startsWith("sub", pos)) {
            pos += 3; // Skip 'sub'
            skipWhitespace();
            String subroutineName = parseIdentifier();
            skipWhitespace();
            List<String> parameters = new ArrayList<>();
            if (code.charAt(pos) == '(') {
                pos++; // Skip '('
                skipWhitespace();
                while (code.charAt(pos) != ')') {
                    parameters.add(parseIdentifier());
                    skipWhitespace();
                    if (code.charAt(pos) == ',') {
                        pos++; // Skip ','
                        skipWhitespace();
                    } else if (code.charAt(pos) != ')') {
                        throw new RuntimeException("Expected ',' or ')'");
                    }
                }
                pos++; // Skip ')'
            }
            skipWhitespace();
            if (code.charAt(pos) == '{') {
                pos++; // Skip '{'
                Node body = parseBlock();
                return new SubroutineDeclarationNode(subroutineName, parameters, body);
            } else {
                throw new RuntimeException("Expected '{'");
            }
        } else {
            throw new RuntimeException("Expected 'sub'");
        }
    }

    private Node parseBlock() {
        List<Node> statements = new ArrayList<>();
        while (true) {
            skipWhitespace();
            if (pos >= code.length() || code.charAt(pos) == '}') {
                break;
            }
            statements.add(parseStatement());
        }
        if (pos < code.length() && code.charAt(pos) == '}') {
            pos++; // Skip '}'
        } else {
            throw new RuntimeException("Expected '}'");
        }
        return new BlockNode(statements);
    }

    private Node parseStatement() {
        skipWhitespace();
        if (code.startsWith("return", pos)) {
            pos += 6; // Skip 'return'
            skipWhitespace();
            Node returnValue = parseExpression();
            skipWhitespace();
            if (code.charAt(pos) == ';') {
                pos++; // Skip ';'
            }
            return new ReturnNode(returnValue);
        } else {
            Node expr = parseExpression();
            skipWhitespace();
            if (code.charAt(pos) == ';') {
                pos++; // Skip ';'
            }
            return expr; // For simplicity, return the expression as the statement
        }
    }

    private Node parseExpression() {
        skipWhitespace();
        Node left = parsePrimary();

        while (true) {
            skipWhitespace();
            if (pos >= code.length()) break;

            char op = code.charAt(pos);
            if (op == '+' || op == '-' || op == '*' || op == '/' || op == '%') {
                pos++; // Skip the operator
                Node right = parsePrimary();
                left = new BinaryOpNode(op, left, right);
            } else if (op == '(') {
                pos++; // Skip '('
                Node argument = parseExpression();
                skipWhitespace();
                if (code.charAt(pos) == ')') {
                    pos++; // Skip ')'
                    return new SubroutineCallNode(((IdentifierNode) left).getName(), argument);
                } else {
                    throw new RuntimeException("Expected ')'");
                }
            } else {
                break;
            }
        }

        return left;
    }

    private Node parsePrimary() {
        skipWhitespace();

        if (pos >= code.length()) {
            throw new RuntimeException("Unexpected end of input");
        }

        char ch = code.charAt(pos);
        if (Character.isDigit(ch)) {
            return parseNumber();
        } else if (Character.isLetter(ch)) {
            return new IdentifierNode(parseIdentifier());
        } else {
            throw new RuntimeException("Unexpected character: " + ch);
        }
    }

    private NumberNode parseNumber() {
        skipWhitespace();
        StringBuilder sb = new StringBuilder();
        while (pos < code.length() && Character.isDigit(code.charAt(pos))) {
            sb.append(code.charAt(pos));
            pos++;
        }
        return new NumberNode(Integer.parseInt(sb.toString()));
    }

    private String parseIdentifier() {
        skipWhitespace();
        StringBuilder sb = new StringBuilder();
        while (pos < code.length() && Character.isLetterOrDigit(code.charAt(pos))) {
            sb.append(code.charAt(pos));
            pos++;
        }
        return sb.toString();
    }

    private void skipWhitespace() {
        while (pos < code.length() && Character.isWhitespace(code.charAt(pos))) {
            pos++;
        }
    }
}


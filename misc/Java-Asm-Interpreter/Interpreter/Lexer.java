import java.util.*;

public class Lexer {
    private final String input;
    private int pos;
    private final int length;

    public Lexer(String input) {
        this.input = input;
        this.pos = 0;
        this.length = input.length();
    }

    public List<Token> tokenize() {
        List<Token> tokens = new ArrayList<>();
        while (pos < length) {
            char current = peek();
            if (Character.isWhitespace(current)) {
                next();
            } else if (Character.isLetter(current)) {
                tokens.add(readIdentifier());
            } else if (Character.isDigit(current)) {
                tokens.add(readNumber());
            } else {
                switch (current) {
                    case '+':
                    case '-':
                    case '*':
                    case '/':
                    case '%':
                        tokens.add(new Token(TokenType.OPERATOR, String.valueOf(next())));
                        break;
                    case '(':
                        tokens.add(new Token(TokenType.LPAREN, String.valueOf(next())));
                        break;
                    case ')':
                        tokens.add(new Token(TokenType.RPAREN, String.valueOf(next())));
                        break;
                    case '{':
                        tokens.add(new Token(TokenType.LBRACE, String.valueOf(next())));
                        break;
                    case '}':
                        tokens.add(new Token(TokenType.RBRACE, String.valueOf(next())));
                        break;
                    case ';':
                        tokens.add(new Token(TokenType.SEMICOLON, String.valueOf(next())));
                        break;
                    default:
                        throw new RuntimeException("Unexpected character: " + current);
                }
            }
        }
        return tokens;
    }

    private Token readIdentifier() {
        StringBuilder sb = new StringBuilder();
        while (pos < length && Character.isLetterOrDigit(peek())) {
            sb.append(next());
        }
        String identifier = sb.toString();
        if (identifier.equals("sub") || identifier.equals("print") || identifier.equals("return")) {
            return new Token(TokenType.KEYWORD, identifier);
        } else {
            return new Token(TokenType.IDENTIFIER, identifier);
        }
    }

    private Token readNumber() {
        StringBuilder sb = new StringBuilder();
        while (pos < length && Character.isDigit(peek())) {
            sb.append(next());
        }
        return new Token(TokenType.NUMBER, sb.toString());
    }

    private char peek() {
        return pos < length ? input.charAt(pos) : '\0';
    }

    private char next() {
        return pos < length ? input.charAt(pos++) : '\0';
    }
}

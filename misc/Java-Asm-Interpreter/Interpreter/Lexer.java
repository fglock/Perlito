import java.util.ArrayList;
import java.util.List;

public class Lexer {
    private final String source;
    private int start;
    private int current;

    public Lexer(String source) {
        this.source = source;
        this.start = 0;
        this.current = 0;
    }

    public List<Token> tokenize() {
        List<Token> tokens = new ArrayList<>();
        while (!isAtEnd()) {
            start = current;
            Token token = scanToken();
            if (token != null) {
                tokens.add(token);
            }
        }
        tokens.add(new Token(TokenType.EOF, ""));
        return tokens;
    }

    private boolean isAtEnd() {
        return current >= source.length();
    }

    private Token scanToken() {
        char c = advance();
        switch (c) {
            case '+': return new Token(TokenType.PLUS, "+");
            case '-': return new Token(TokenType.MINUS, "-");
            case '*': return new Token(TokenType.STAR, "*");
            case '/': return new Token(TokenType.SLASH, "/");
            case '(': return new Token(TokenType.LPAREN, "(");
            case ')': return new Token(TokenType.RPAREN, ")");
            case '{': return new Token(TokenType.LBRACE, "{");
            case '}': return new Token(TokenType.RBRACE, "}");
            case ';': return new Token(TokenType.SEMICOLON, ";");
            case ',': return new Token(TokenType.COMMA, ",");
            case ' ':
            case '\r':
            case '\t':
            case '\n':
                return null; // Skip whitespace
            default:
                if (isDigit(c)) {
                    return number();
                } else if (isAlpha(c)) {
                    return identifier();
                } else {
                    throw new RuntimeException("Unexpected character: " + c);
                }
        }
    }

    private char advance() {
        current++;
        return source.charAt(current - 1);
    }

    private Token number() {
        while (isDigit(peek())) advance();
        if (peek() == '.' && isDigit(peekNext())) {
            advance();
            while (isDigit(peek())) advance();
        }
        return new Token(TokenType.NUMBER, source.substring(start, current));
    }

    private Token identifier() {
        while (isAlphaNumeric(peek())) advance();
        String text = source.substring(start, current);
        TokenType type = switch (text) {
            case "return" -> TokenType.RETURN;
            case "print" -> TokenType.PRINT;
            case "sub" -> TokenType.SUB;
            default -> TokenType.IDENTIFIER;
        };
        return new Token(type, text);
    }

    private char peek() {
        if (isAtEnd()) return '\0';
        return source.charAt(current);
    }

    private char peekNext() {
        if (current + 1 >= source.length()) return '\0';
        return source.charAt(current + 1);
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }
}

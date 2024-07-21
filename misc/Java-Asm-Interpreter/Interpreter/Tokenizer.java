import java.util.ArrayList;
import java.util.List;

public class Tokenizer {
    private final String input;
    private int pos = 0;

    public Tokenizer(String input) {
        this.input = input;
    }

    private char peek() {
        return input.charAt(pos);
    }

    private char advance() {
        return input.charAt(pos++);
    }

    private boolean hasNext() {
        return pos < input.length();
    }

    public List<Token> tokenize() {
        List<Token> tokens = new ArrayList<>();

        while (hasNext()) {
            char current = peek();

            if (Character.isDigit(current)) {
                tokens.add(tokenizeNumber());
            } else if (Character.isAlphabetic(current)) {
                tokens.add(tokenizeIdentifier());
            } else if (current == '+') {
                tokens.add(new Token(TokenType.PLUS, Character.toString(advance())));
            } else if (current == '(') {
                tokens.add(new Token(TokenType.LPAREN, Character.toString(advance())));
            } else if (current == ')') {
                tokens.add(new Token(TokenType.RPAREN, Character.toString(advance())));
            } else if (current == '{') {
                tokens.add(new Token(TokenType.LBRACE, Character.toString(advance())));
            } else if (current == '}') {
                tokens.add(new Token(TokenType.RBRACE, Character.toString(advance())));
            } else if (current == ',') {
                tokens.add(new Token(TokenType.COMMA, Character.toString(advance())));
            } else if (current == ';') {
                tokens.add(new Token(TokenType.SEMICOLON, Character.toString(advance())));
            } else if (Character.isWhitespace(current)) {
                advance();  // Skip whitespace
            } else {
                throw new RuntimeException("Unexpected character: " + current);
            }
        }

        tokens.add(new Token(TokenType.EOF, ""));
        return tokens;
    }

    private Token tokenizeNumber() {
        StringBuilder number = new StringBuilder();
        while (hasNext() && Character.isDigit(peek())) {
            number.append(advance());
        }
        return new Token(TokenType.NUMBER, number.toString());
    }

    private Token tokenizeIdentifier() {
        StringBuilder identifier = new StringBuilder();
        while (hasNext() && Character.isAlphabetic(peek())) {
            identifier.append(advance());
        }
        String value = identifier.toString();
        TokenType type;
        switch (value) {
            case "return":
                type = TokenType.RETURN;
                break;
            case "print":
                type = TokenType.PRINT;
                break;
            case "sub":
                type = TokenType.SUB;
                break;
            default:
                type = TokenType.IDENTIFIER;
                break;
        }
        return new Token(type, value);
    }
}

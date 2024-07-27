import java.util.ArrayList;
import java.util.List;

public class PerlLexer {
  private static final char EOF = (char) -1;
  private final char[] input;
  private int position;
  private int length;
  private static boolean isOperator[];

  static {
    isOperator = new boolean[128];
    for (char c : "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~".toCharArray()) {
      isOperator[c] = true;
    }
  }

  public enum TokenType {
    WHITESPACE,
    NEWLINE,
    IDENTIFIER,
    NUMBER,
    OPERATOR,
    STRING
  }

  public static class Token {
    public final TokenType type;
    public final String text;

    public Token(TokenType type, String text) {
      this.type = type;
      this.text = text;
    }

    @Override
    public String toString() {
      return "Token{" + "type=" + type + ", text='" + text + '\'' + '}';
    }
  }

  public PerlLexer(String input) {
    this.input = input.toCharArray();
    this.length = this.input.length;
    this.position = 0;
  }

  public List<Token> tokenize() {
    List<Token> tokens = new ArrayList<>();
    Token token;

    while ((token = nextToken()) != null) {
      tokens.add(token);
    }

    return tokens;
  }

  private Token nextToken() {
    if (position >= length) {
      return null;
    }

    char current = input[position];

    if (Character.isWhitespace(current)) {
      if (current == '\n') {
        position++;
        return new Token(TokenType.NEWLINE, "\n");
      } else {
        return consumeWhitespace();
      }
    } else if (Character.isDigit(current)) {
      return consumeNumber();
    } else if (Character.isLetter(current) || current == '_') {
      return consumeIdentifier();
    } else if (current < 128 && isOperator[current]) {
      return consumeOperator();
    } else {
      position++;
      return new Token(TokenType.STRING, String.valueOf(current));
    }
  }

  private Token consumeWhitespace() {
    int start = position;
    while (position < length
        && Character.isWhitespace(input[position])
        && input[position] != '\n') {
      position++;
    }
    return new Token(TokenType.WHITESPACE, new String(input, start, position - start));
  }

  private Token consumeNumber() {
    int start = position;
    while (position < length && Character.isDigit(input[position])) {
      position++;
    }
    return new Token(TokenType.NUMBER, new String(input, start, position - start));
  }

  private Token consumeIdentifier() {
    int start = position;
    while (position < length
        && (Character.isLetterOrDigit(input[position]) || input[position] == '_')) {
      position++;
    }
    return new Token(TokenType.IDENTIFIER, new String(input, start, position - start));
  }

  private Token consumeOperator() {
    int start = position;
    char current = input[position];
    position++;

    if (position < length) {
      char next = input[position];
      switch (current) {
        case '!':
          if (next == '=' || next == '~') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '$':
          if (next == '#') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '%':
          if (next == '=') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '&':
          if (next == '&') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          if (next == '=' || next == '.') {
            position++;
            if (position < length && input[position] == '=') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '*':
          if (next == '*' || next == '=') {
            position++;
            if (next == '*' && position < length && input[position] == '=') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '+':
          if (next == '+' || next == '=') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '-':
          if (next == '-' || next == '>' || next == '=') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '.':
          if (next == '.' || next == '=') {
            position++;
            if (next == '.' && position < length && input[position] == '.') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '/':
          if (next == '/' || next == '=') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case ':':
          if (next == ':') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '<':
          if (next == '=' || next == '<') {
            position++;
            if (next == '<' && position < length && input[position] == '=') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          if (next == '=') {
            position++;
            if (position < length && input[position] == '>') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
          }
          break;
        case '=':
          if (next == '=' || next == '>') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '>':
          if (next == '=' || next == '>') {
            position++;
            if (next == '>' && position < length && input[position] == '=') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '^':
          if (next == '=' || next == '^') {
            position++;
            if (next == '.' && position < length && input[position] == '=') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '|':
          if (next == '=' || next == '|') {
            position++;
            if (next == '.' && position < length && input[position] == '=') {
              position++;
              return new Token(TokenType.OPERATOR, new String(input, start, 3));
            }
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case '~':
          if (next == '~') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
        case 'x':
          if (next == '=') {
            position++;
            return new Token(TokenType.OPERATOR, new String(input, start, 2));
          }
          break;
      }
    }

    return new Token(TokenType.OPERATOR, new String(input, start, 1));
  }

  public static void main(String[] args) {
    String code =
        "my $var = 42; print \"Hello, World!\\n\"; $a == $b; qq{ x \" y € z }; "
            + " &&= &.= **= ... //= <<= <=> >>= ^.= |.= ||= ";
    PerlLexer lexer = new PerlLexer(code);
    List<Token> tokens = lexer.tokenize();

    for (Token token : tokens) {
      System.out.println(token);
    }
  }
}

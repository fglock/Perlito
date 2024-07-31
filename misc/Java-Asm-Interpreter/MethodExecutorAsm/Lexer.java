import java.util.ArrayList;
import java.util.List; 

/**
 * The Lexer class is responsible for converting a sequence of characters (input string)
 * into a sequence of tokens. This process is known as lexical analysis or tokenization.
 * 
 * In the context of programming languages, a lexer (or lexical analyzer) is the first
 * phase of a compiler or interpreter. It reads the input source code and breaks it down
 * into meaningful elements called tokens. Each token represents a basic building block
 * of the language, such as keywords, operators, identifiers, literals, and punctuation.
 * 
 * The Lexer class in this example is designed to handle a subset of Perl-like syntax,
 * including identifiers, operators, and string literals. It uses a character array to
 * process the input string and identifies operators using a boolean array.
 * 
 * The main responsibilities of the Lexer class include:
 * - Reading and processing the input string.
 * - Identifying and categorizing different types of tokens.
 * - Handling special characters and operators.
 * - Providing a list of tokens that can be used by subsequent phases of a compiler or interpreter.
 */ 
public class Lexer {
  // End of File character constant
  public static final String EOF = Character.toString((char) -1);
  
  // Input characters to be tokenized
  public final char[] input;
  
  // Current position in the input
  public int position;
  
  // Length of the input
  public int length;
  
  // Array to mark operator characters
  public static boolean isOperator[];
          
  // Static block to initialize the isOperator array
  static {
    isOperator = new boolean[128];
    // Marking specific characters as operators
    for (char c : "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~".toCharArray()) {
      isOperator[c] = true;
    }
  }

  // Constructor to initialize the Lexer with input string
  public Lexer(String input) {
    this.input = input.toCharArray();
    this.length = this.input.length;
    this.position = 0;
  } 
    
  // Method to tokenize the input string into a list of tokens
  public List<Token> tokenize() {
    List<Token> tokens = new ArrayList<>();
    Token token;

    while ((token = nextToken()) != null) {
      tokens.add(token);
    }
    tokens.add(new Token(TokenType.EOF, EOF));
    tokens.add(new Token(TokenType.EOF, EOF));

    return tokens;
  }

  public Token nextToken() {
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

  public Token consumeWhitespace() {
    int start = position;
    while (position < length
        && Character.isWhitespace(input[position])
        && input[position] != '\n') {
      position++;
    }
    return new Token(TokenType.WHITESPACE, new String(input, start, position - start));
  }

  public Token consumeNumber() {
    int start = position;
    while (position < length && Character.isDigit(input[position])) {
      position++;
    }
    return new Token(TokenType.NUMBER, new String(input, start, position - start));
  }

  public Token consumeIdentifier() {
    int start = position;
    while (position < length
        && (Character.isLetterOrDigit(input[position]) || input[position] == '_')) {
      position++;
    }
    return new Token(TokenType.IDENTIFIER, new String(input, start, position - start));
  }

  public Token consumeOperator() {
    int start = position;
    char current = input[position];
    if (position < length && (current < 128 && isOperator[current])) {
      switch (current) {
        case '!':
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "!=");
          }
          if (position + 2 <= input.length && input[position + 1] == '~') {
            position += 2;
            return new Token(TokenType.OPERATOR, "!~");
          }
          break;
        case '$':
          if (position + 2 <= input.length && input[position + 1] == '#') {
            position += 2;
            return new Token(TokenType.OPERATOR, "$#");
          }
          break;
        case '%':
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "%=");
          }
          break;
        case '&':
          if (position + 3 <= input.length
              && input[position + 1] == '&'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "&&=");
          }
          if (position + 3 <= input.length
              && input[position + 1] == '.'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "&.=");
          }
          if (position + 2 <= input.length && input[position + 1] == '&') {
            position += 2;
            return new Token(TokenType.OPERATOR, "&&");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "&=");
          }
          break;
        case '*':
          if (position + 3 <= input.length
              && input[position + 1] == '*'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "**=");
          }
          if (position + 2 <= input.length && input[position + 1] == '*') {
            position += 2;
            return new Token(TokenType.OPERATOR, "**");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "*=");
          }
          break;
        case '+':
          if (position + 2 <= input.length && input[position + 1] == '+') {
            position += 2;
            return new Token(TokenType.OPERATOR, "++");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "+=");
          }
          break;
        case '-':
          if (position + 2 <= input.length && input[position + 1] == '-') {
            position += 2;
            return new Token(TokenType.OPERATOR, "--");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "-=");
          }
          if (position + 2 <= input.length && input[position + 1] == '>') {
            position += 2;
            return new Token(TokenType.OPERATOR, "->");
          }
          break;
        case '.':
          if (position + 3 <= input.length
              && input[position + 1] == '.'
              && input[position + 2] == '.') {
            position += 3;
            return new Token(TokenType.OPERATOR, "...");
          }
          if (position + 2 <= input.length && input[position + 1] == '.') {
            position += 2;
            return new Token(TokenType.OPERATOR, "..");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, ".=");
          }
          break;
        case '/':
          if (position + 3 <= input.length
              && input[position + 1] == '/'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "//=");
          }
          if (position + 2 <= input.length && input[position + 1] == '/') {
            position += 2;
            return new Token(TokenType.OPERATOR, "//");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "/=");
          }
          break;
        case ':':
          if (position + 2 <= input.length && input[position + 1] == ':') {
            position += 2;
            return new Token(TokenType.OPERATOR, "::");
          }
          break;
        case '<':
          if (position + 3 <= input.length
              && input[position + 1] == '<'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "<<=");
          }
          if (position + 3 <= input.length
              && input[position + 1] == '='
              && input[position + 2] == '>') {
            position += 3;
            return new Token(TokenType.OPERATOR, "<=>");
          }
          if (position + 2 <= input.length && input[position + 1] == '<') {
            position += 2;
            return new Token(TokenType.OPERATOR, "<<");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "<=");
          }
          break;
        case '=':
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "==");
          }
          if (position + 2 <= input.length && input[position + 1] == '>') {
            position += 2;
            return new Token(TokenType.OPERATOR, "=>");
          }
          if (position + 2 <= input.length && input[position + 1] == '~') {
            position += 2;
            return new Token(TokenType.OPERATOR, "=~");
          }
          break;
        case '>':
          if (position + 3 <= input.length
              && input[position + 1] == '>'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, ">>=");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, ">=");
          }
          if (position + 2 <= input.length && input[position + 1] == '>') {
            position += 2;
            return new Token(TokenType.OPERATOR, ">>");
          }
          break;
        case '^':
          if (position + 3 <= input.length
              && input[position + 1] == '.'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "^.=");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "^=");
          }
          if (position + 2 <= input.length && input[position + 1] == '^') {
            position += 2;
            return new Token(TokenType.OPERATOR, "^^");
          }
          break;
        case 'x':
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "x=");
          }
          break;
        case '|':
          if (position + 3 <= input.length
              && input[position + 1] == '.'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "|.=");
          }
          if (position + 3 <= input.length
              && input[position + 1] == '|'
              && input[position + 2] == '=') {
            position += 3;
            return new Token(TokenType.OPERATOR, "||=");
          }
          if (position + 2 <= input.length && input[position + 1] == '=') {
            position += 2;
            return new Token(TokenType.OPERATOR, "|=");
          }
          if (position + 2 <= input.length && input[position + 1] == '|') {
            position += 2;
            return new Token(TokenType.OPERATOR, "||");
          }
          break;
        case '~':
          if (position + 2 <= input.length && input[position + 1] == '~') {
            position += 2;
            return new Token(TokenType.OPERATOR, "~~");
          }
          break;
      }
    }

    position++;
    return new Token(TokenType.OPERATOR, new String(input, start, 1));
  }

  // Main method for testing the Lexer
  public static void main(String[] args) {
    // Sample code to be tokenized
    String code =
        "my $var = 42; print \"Hello, World!\\n\"; $a == $b; qq{ x \" y € z }; "
            + " &&= &.= **= ... //= <<= <=> >>= ^.= |.= ||= ";
    if (args.length >= 2 && args[0].equals("-e")) {
      code = args[1]; // Read the code from the command line parameter
    }
    
    // Creating a Lexer instance with the sample code
    Lexer lexer = new Lexer(code); 
    
    // Tokenizing the input code
    List<Token> tokens = lexer.tokenize();
    
    // Printing the tokens
    for (Token token : tokens) {
      System.out.println(token);
    }
  } 
}


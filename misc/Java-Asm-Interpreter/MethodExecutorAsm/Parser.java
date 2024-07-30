import java.util.*;

public class Parser {
  private final List<Token> tokens;
  private int position = 0;
  private static final Set<String> TERMINATORS =
      new HashSet<>(Arrays.asList(":", ";", ")", "}", "]"));
  private static final Set<String> UNARY_OP =
      new HashSet<>(
          Arrays.asList(
              "!",
              "\\",
              "-",
              "+",
              "--",
              "++", // operators
              "$",
              "@",
              "%",
              "*",
              "&",
              "$#" // sigils
              ));

  public Parser(List<Token> tokens) {
    this.tokens = tokens;
  }

  public Node parse() {
    return parseBlock();
  }

  private Node parseBlock() {
      List<Node> statements = new ArrayList<>();
      Token token = peek();
      while (token.type != TokenType.EOF && !(token.type == TokenType.OPERATOR && token.text.equals("}"))) {
          if (token.text.equals(";")) {
            consume();
          }
          else {
            statements.add(parseStatement());
          }
          token = peek();
      }
      return new BlockNode(statements);
  }

  public Node parseStatement() {
      Token token = peek();

      // if (token.type == IDENTIFIER) {
      //     switch (token.text) {
      //       case "if": {
      //         return ...
      //       }
      //    }
      // }
      if (token.type == TokenType.OPERATOR && token.text.equals("{")) {
        consume(TokenType.OPERATOR, "{");
        Node block = parseBlock();
        consume(TokenType.OPERATOR, "}");
        return block;
      }
      Node expression = parseExpression(0);
      token = peek();
      System.out.println("after expression: " + token);
      if (token.type != TokenType.EOF && !token.text.equals("}") && !token.text.equals(";")) {
        throw new RuntimeException("Unexpected token: " + token);
      }
      if (token.text.equals(";")) {
        consume();
      }
      return expression;
  }

  // private Node parseIfStatement() {
  //   consume(TokenType.IF);
  //   Node condition = parseExpression(0);
  //   consume(TokenType.OPERATOR, "{");
  //   Node thenBranch = parseBlock();
  //   consume(TokenType.OPERATOR, "}");
  //   Node elseBranch = null;
  //   if (match(TokenType.ELSE)) {
  //       consume(TokenType.OPERATOR, "{");
  //       elseBranch = parseBlock();
  //       consume(TokenType.OPERATOR, "}");
  //   }
  //   return new IfNode(condition, thenBranch, elseBranch);
  // }

  private Node parseExpression(int precedence) {
    Node left = parsePrimary();

    while (true) {
      Token token = peek();
      if (token.type == TokenType.EOF || TERMINATORS.contains(token.text)) {
        break;
      }

      int tokenPrecedence = getPrecedence(token);

      if (tokenPrecedence < precedence) {
        break;
      }

      if (isRightAssociative(token)) {
        left = parseInfix(left, tokenPrecedence - 1);
      } else {
        left = parseInfix(left, tokenPrecedence);
      }
    }

    return left;
  }

  private Node parsePrimary() {
    Token token = consume();

    switch (token.type) {
      case IDENTIFIER:
        if (token.text.equals("my")) {
          Node operand = parsePrimary();
          return new UnaryOperatorNode("my", operand);
        }
        if (token.text.equals("return")) {
          Node operand = parsePrimary();
          return new UnaryOperatorNode("return", operand);
        }
        return new IdentifierNode(token.text);
      case NUMBER:
        return parseNumber(token);
      case STRING:
        return new StringNode(token.text);
      case OPERATOR:
        if (token.text.equals("(")) {
          Node expr = parseExpression(0);
          consume(TokenType.OPERATOR, ")");
          return expr;
        } else if (UNARY_OP.contains(token.text)) {
          Node operand = parsePrimary();
          return new UnaryOperatorNode(token.text, operand);
        } else if (token.text.equals(".")) {
          return parseFractionalNumber();
        }
        break;
      case EOF:
        return null; // End of input
      default:
        throw new RuntimeException("Unexpected token: " + token);
    }
    throw new RuntimeException("Unexpected token: " + token);
  }

  private Node parseNumber(Token token) {
    StringBuilder number = new StringBuilder(token.text);

    // Check for fractional part
    if (peek().text.equals(".")) {
      number.append(consume().text); // consume '.'
      number.append(consume(TokenType.NUMBER).text); // consume digits after '.'
    }

    // Check for exponent part
    if (peek().text.equals("e") || peek().text.equals("E")) {
      number.append(consume().text); // consume 'e' or 'E'
      if (peek().text.equals("-") || peek().text.equals("+")) {
        number.append(consume().text); // consume '-' or '+'
      }
      number.append(consume(TokenType.NUMBER).text); // consume exponent digits
    }

    return new NumberNode(number.toString());
  }

  private Node parseFractionalNumber() {
    StringBuilder number = new StringBuilder("0.");

    number.append(consume(TokenType.NUMBER).text); // consume digits after '.'

    // Check for exponent part
    if (peek().text.equals("e") || peek().text.equals("E")) {
      number.append(consume().text); // consume 'e' or 'E'
      if (peek().text.equals("-") || peek().text.equals("+")) {
        number.append(consume().text); // consume '-' or '+'
      }
      number.append(consume(TokenType.NUMBER).text); // consume exponent digits
    }

    return new NumberNode(number.toString());
  }

  private Node parseInfix(Node left, int precedence) {
    Token token = consume();

    if (isTernaryOperator(token)) {
      Node middle = parseExpression(0);
      consume(TokenType.OPERATOR, ":");
      Node right = parseExpression(precedence);
      return new TernaryOperatorNode(token.text, left, middle, right);
    }

    Node right = parseExpression(precedence);

    switch (token.text) {
      case "+":
      case "-":
      case "*":
      case "/":
      case "=":
        return new BinaryOperatorNode(token.text, left, right);
      case "->":
        return new PostfixOperatorNode(token.text, left);
        // Handle other infix operators
      default:
        throw new RuntimeException("Unexpected infix operator: " + token);
    }
  }

  private Token peek() {
    while (position < tokens.size()
        && (tokens.get(position).type == TokenType.WHITESPACE
            || tokens.get(position).type == TokenType.NEWLINE)) {
      position++;
    }
    if (position >= tokens.size()) {
      return new Token(TokenType.EOF, "");
    }
    return tokens.get(position);
  }

  private Token consume() {
    while (position < tokens.size()
        && (tokens.get(position).type == TokenType.WHITESPACE
            || tokens.get(position).type == TokenType.NEWLINE)) {
      position++;
    }
    if (position >= tokens.size()) {
      return new Token(TokenType.EOF, "");
    }
    return tokens.get(position++);
  }

  private Token consume(TokenType type) {
    Token token = consume();
    if (token.type != type) {
      throw new RuntimeException("Expected token " + type + " but got " + token);
    }
    return token;
  }

  private void consume(TokenType type, String text) {
    Token token = consume();
    if (token.type != type || !token.text.equals(text)) {
      throw new RuntimeException(
          "Expected token " + type + " with text " + text + " but got " + token);
    }
  }

  private int getPrecedence(Token token) {
    // Define precedence levels for operators
    switch (token.text) {
      case "=":
        return 1; // Lower precedence for assignment
      case "?":
        return 2; // Precedence for ternary operator
      case "+":
      case "-":
        return 3;
      case "*":
      case "/":
        return 4;
      case "->":
        return 5;
      case "$":
        return 6; // Higher precedence for prefix operator
      default:
        return 0;
    }
  }

  private boolean isRightAssociative(Token token) {
    // Define right associative operators
    switch (token.text) {
      case "=":
      case "?":
        return true;
      default:
        return false;
    }
  }

  private boolean isTernaryOperator(Token token) {
    return token.text.equals("?");
  }

  // Example of a method to parse a list
  private Node parseList() {
    List<Node> elements = new ArrayList<>();
    consume(TokenType.OPERATOR, "(");
    while (!peek().text.equals(")")) {
      elements.add(parseExpression(0));
      if (peek().text.equals(",")) {
        consume();
      }
    }
    consume(TokenType.OPERATOR, ")");
    return new ListNode(elements);
  }

  public static String getASTString(Node node) {
    PrintVisitor printVisitor = new PrintVisitor();
    node.accept(printVisitor);
    return printVisitor.getResult();
  }

  public static void main(String[] args) {
    String code = "my $var = 42; 1 ? 2 : 3; print \"Hello, World!\\n\";";
    if (args.length >= 2 && args[0].equals("-e")) {
      code = args[1]; // Read the code from the command line parameter
    }
    Lexer lexer = new Lexer(code);
    List<Token> tokens = lexer.tokenize();
    Parser parser = new Parser(tokens);
    Node ast = parser.parse();
    System.out.println(getASTString(ast));
  }
}

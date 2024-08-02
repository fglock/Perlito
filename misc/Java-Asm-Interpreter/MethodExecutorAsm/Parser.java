import java.util.*;

public class Parser {
  private final List<Token> tokens;
  private final ErrorMessageUtil errorUtil;
  private int tokenIndex = 0;
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

  public Parser(ErrorMessageUtil errorUtil, List<Token> tokens) {
    this.errorUtil = errorUtil;
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
      return new BlockNode(statements, tokenIndex);
  }

  public Node parseStatement() {
      Token token = peek();

      if (token.type == TokenType.IDENTIFIER) {
          switch (token.text) {
            case "if": {
              return parseIfStatement();
            }
         }
      }
      if (token.type == TokenType.OPERATOR && token.text.equals("{")) { // bare-block
        consume(TokenType.OPERATOR, "{");
        Node block = parseBlock();
        consume(TokenType.OPERATOR, "}");
        return block;
      }
      Node expression = parseExpression(0);
      token = peek();
      if (token.type != TokenType.EOF && !token.text.equals("}") && !token.text.equals(";")) {
        throw new PerlCompilerException(tokenIndex, "Unexpected token: " + token, errorUtil);
      }
      if (token.text.equals(";")) {
        consume();
      }
      return expression;
  }

  private Node parseAnonSub(Token token) {
    // token == "sub"
    // TODO - optional name, subroutine prototype
    consume(TokenType.OPERATOR, "{");
    Node block = parseBlock();
    consume(TokenType.OPERATOR, "}");
    return new AnonSubNode(block, tokenIndex);
  }

  private Node parseIfStatement() {
    consume(TokenType.IDENTIFIER);  // "if", "elsif"
    consume(TokenType.OPERATOR, "(");
    Node condition = parseExpression(0);
    consume(TokenType.OPERATOR, ")");
    consume(TokenType.OPERATOR, "{");
    Node thenBranch = parseBlock();
    consume(TokenType.OPERATOR, "}");
    Node elseBranch = null;
    Token token = peek();
    if (token.text.equals("else")) {
        consume(TokenType.IDENTIFIER);  // "else"
        consume(TokenType.OPERATOR, "{");
        elseBranch = parseBlock();
        consume(TokenType.OPERATOR, "}");
    }
    else if (token.text.equals("elsif")) {
        elseBranch = parseIfStatement();
    }
    return new IfNode(condition, thenBranch, elseBranch, tokenIndex);
  }

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
        if (token.text.equals("not")) {
          Node operand = parseExpression(getPrecedence(token) + 1);
          return new UnaryOperatorNode("not", operand, tokenIndex);
        }
        if (token.text.equals("print")) {
          Node operand = parsePrimary();
          return new UnaryOperatorNode("print", operand, tokenIndex);
        }
        if (token.text.equals("my")) {
          Node operand = parsePrimary();
          return new UnaryOperatorNode("my", operand, tokenIndex);
        }
        if (token.text.equals("return")) {
          Node operand = parsePrimary();
          return new UnaryOperatorNode("return", operand, tokenIndex);
        }
        if (token.text.equals("eval")) {
          Node operand;
          token = peek();
          if (token.type == TokenType.OPERATOR && token.text.equals("{")) { // eval block
            consume(TokenType.OPERATOR, "{");
            operand = parseBlock();
            consume(TokenType.OPERATOR, "}");
          }
          else { // eval string
            operand = parsePrimary();
          }
          return new UnaryOperatorNode("eval", operand, tokenIndex);
        }
        if (token.text.equals("do")) {
          token = peek();
          if (token.type == TokenType.OPERATOR && token.text.equals("{")) {
            consume(TokenType.OPERATOR, "{");
            Node block = parseBlock();
            consume(TokenType.OPERATOR, "}");
            return block;
          }
        }
        if (token.text.equals("sub")) {
          return parseAnonSub(token);
        }
        return new IdentifierNode(token.text, tokenIndex);
      case NUMBER:
        return parseNumber(token);
      case STRING:
        return new StringNode(token.text, tokenIndex);
      case OPERATOR:
        if (token.text.equals("(")) {
          Node expr = parseExpression(0);
          consume(TokenType.OPERATOR, ")");
          return expr;
        } else if (UNARY_OP.contains(token.text)) {
          Node operand = parseExpression(getPrecedence(token) + 1);
          return new UnaryOperatorNode(token.text, operand, tokenIndex);
        } else if (token.text.equals(".")) {
          return parseFractionalNumber();
        } else if (token.text.equals("'")) {
          return parseSingleQuotedString();
        } else if (token.text.equals("\"")) {
          return parseDoubleQuotedString();
        }
        break;
      case EOF:
        return null; // End of input
      default:
        throw new PerlCompilerException(tokenIndex, "Unexpected token: " + token, errorUtil);
    }
    throw new PerlCompilerException(tokenIndex, "Unexpected token: " + token, errorUtil);
  }

    private Node parseDoubleQuotedString() {
        StringBuilder str = new StringBuilder();
        List<Node> parts = new ArrayList<>();
        Token token = tokens.get(tokenIndex);
        while (!token.text.equals("\"")) {
            tokenIndex++;
            String text = token.text;
            if (token.type == TokenType.OPERATOR) {
                if (text.equals("\\")) {
                    // Handle escaped characters
                    text = consume().text;  // XXX TODO the string is tokenized, we need to check single characters
                    switch (text) {
                        case "\\":
                        case "\"":
                            str.append(text);
                            break;
                        case "n":
                            str.append("\n");
                            break;
                        case "t":
                            str.append("\t");
                            break;
                        default:
                            str.append(text);
                            break;
                    }
                } else if (text.equals("$")) {
                    if (str.length() > 0) {
                        parts.add(new StringNode(str.toString(), tokenIndex));  // string so far
                        str = new StringBuilder();  // continue
                    }
                    Token nextToken = peek();
                    if (nextToken.type == TokenType.IDENTIFIER) {
                        parts.add(new UnaryOperatorNode("$", new IdentifierNode(consume().text, tokenIndex), tokenIndex));
                    } else if (nextToken.type == TokenType.OPERATOR && nextToken.text.equals("{")) {
                        consume(); // consume '{'
                        String varName = consume(TokenType.IDENTIFIER).text;
                        consume(TokenType.OPERATOR, "}"); // consume '}'
                        parts.add(new UnaryOperatorNode("$", new IdentifierNode(varName, tokenIndex), tokenIndex));
                    } else {
                        throw new RuntimeException("Final $ should be \\$ or $name");
                    }
                } else {
                    str.append(text);
                }
            } else {
                str.append(text);
            }
            token = tokens.get(tokenIndex);
        }
        if (str.length() > 0) {
            parts.add(new StringNode(str.toString(), tokenIndex));
        }
        consume(TokenType.OPERATOR, "\""); // Consume the closing double quote
        
        // Join the parts
        if (parts.size() == 1) {
            return parts.get(0);    // TODO make sure to stringify
        } else {
            Node result = parts.get(0);
            for (int i = 1; i < parts.size(); i++) {
                result = new BinaryOperatorNode(".", result, parts.get(i), tokenIndex);
            }
            return result;
        }
    }

  private Node parseSingleQuotedString() {
      StringBuilder str = new StringBuilder();
      while (!peek().text.equals("'")) {
          String text = consume().text;
          if (text.equals("\\")) {
              // Handle escaped characters
              text = consume().text;
              if (text.equals("\\") || text.equals("'")) {
                  str.append(text);
              } else {
                  str.append("\\").append(text);
              }
          } else {
              str.append(text);
          }
      }
      consume(TokenType.OPERATOR, "'"); // Consume the closing single quote
      return new StringNode(str.toString(), tokenIndex);
  }

  private Node parseNumber(Token token) {
    StringBuilder number = new StringBuilder(token.text);

    // Check for fractional part
    if (tokens.get(tokenIndex).text.equals(".")) {
      number.append(consume().text); // consume '.'
      if (tokens.get(tokenIndex).type == TokenType.NUMBER) {
        number.append(consume().text); // consume digits after '.'
      }
    }
    // Check for exponent part
    checkNumberExponent(number);

    return new NumberNode(number.toString(), tokenIndex);
  }

  private Node parseFractionalNumber() {
    StringBuilder number = new StringBuilder("0.");

    number.append(consume(TokenType.NUMBER).text); // consume digits after '.'
    // Check for exponent part
    checkNumberExponent(number);
    return new NumberNode(number.toString(), tokenIndex);
  }

  private void checkNumberExponent(StringBuilder number) {
    // Check for exponent part
    if (tokens.get(tokenIndex).text.startsWith("e") || tokens.get(tokenIndex).text.startsWith("E")) {
        String exponentPart = consume().text; // consume 'e' or 'E' and possibly more 'E10'
        number.append(exponentPart.charAt(0)); // append 'e' or 'E'

        int index = 1;
        // Check if the rest of the token contains digits (e.g., "E10")
        while (index < exponentPart.length()) {
            if (!Character.isDigit(exponentPart.charAt(index))) {
                throw new PerlCompilerException(tokenIndex, "Malformed number", errorUtil);
            }
            number.append(exponentPart.charAt(index));
            index++;
        }

        // If the exponent part was not fully consumed, check for separate tokens
        if (index == 1) {
            // Check for optional sign
            if (tokens.get(tokenIndex).text.equals("-") || tokens.get(tokenIndex).text.equals("+")) {
                number.append(consume().text); // consume '-' or '+'
            }

            // Consume exponent digits
            number.append(consume(TokenType.NUMBER).text);
        }
    }
  }

  private Node parseInfix(Node left, int precedence) {
    Token token = consume();

    if (isTernaryOperator(token)) {
      Node middle = parseExpression(0);
      consume(TokenType.OPERATOR, ":");
      Node right = parseExpression(precedence);
      return new TernaryOperatorNode(token.text, left, middle, right, tokenIndex);
    }

    Node right;
    switch (token.text) {
      case "or":
      case "xor":
      case "and":
      case "||":
      case "//":
      case "&&":
      case "==":
      case "!=":
      case "<=>":
      case "eq":
      case "ne":
      case "cmp":
      case "<":
      case ">":
      case "<=":
      case ">=":
      case "lt":
      case "gt":
      case "le":
      case "ge":
      case "+":
      case "-":
      case "*":
      case "**":
      case "/":
      case "%":
      case ".":
      case "=":
      case "=~":
      case "!~":
      case "x":
      case "..":
      case "...":
        right = parseExpression(precedence);
        return new BinaryOperatorNode(token.text, left, right, tokenIndex);
      case "->":
        if (peek().text.equals("(") ) {
            right = parseList();
            return new BinaryOperatorNode(token.text, left, right, tokenIndex);
        }
        right = parseExpression(precedence);
        return new BinaryOperatorNode(token.text, left, right, tokenIndex);
    }
    throw new PerlCompilerException(tokenIndex, "Unexpected infix operator: " + token, errorUtil);
  }

  private Token peek() {
    while (tokenIndex < tokens.size()
        && (tokens.get(tokenIndex).type == TokenType.WHITESPACE
            || tokens.get(tokenIndex).type == TokenType.NEWLINE)) {
      tokenIndex++;
    }
    if (tokenIndex >= tokens.size()) {
      return new Token(TokenType.EOF, "");
    }
    return tokens.get(tokenIndex);
  }

  private Token consume() {
    while (tokenIndex < tokens.size()
        && (tokens.get(tokenIndex).type == TokenType.WHITESPACE
            || tokens.get(tokenIndex).type == TokenType.NEWLINE)) {
      tokenIndex++;
    }
    // if (tokenIndex >= tokens.size()) {
    //   return new Token(TokenType.EOF, "");
    // }
    return tokens.get(tokenIndex++);
  }

  private Token consume(TokenType type) {
    Token token = consume();
    if (token.type != type) {
      throw new PerlCompilerException(tokenIndex, "Expected token " + type + " but got " + token, errorUtil);
    }
    return token;
  }

  private void consume(TokenType type, String text) {
    Token token = consume();
    if (token.type != type || !token.text.equals(text)) {
      throw new PerlCompilerException(tokenIndex, "Expected token " + type + " with text " + text + " but got " + token, errorUtil);
    }
  }

  private int getPrecedence(Token token) {
    // Define precedence levels for operators
    switch (token.text) {
      case "or":
      case "xor":
        return 1;
      case "and":
        return 2;
      case "not":
        return 3;

      case "=":
        return 6; // Lower precedence for assignment
      case "?":
        return 7; // Precedence for ternary operator

      case "||":
      case "^^":
      case "//":
        return 9;
      case "&&":
        return 10;

      case "+":
      case "-":
        return 13;
      case "*":
      case "/":
        return 14;

      case "**":
        return 17;
      case "++":
      case "--":
        return 18;
      case "->":
        return 19;
      case "$":
        return 20; // Higher precedence for prefix operator
      default:
        return 0;
    }
  }

  private boolean isRightAssociative(Token token) {
    // Define right associative operators
    switch (token.text) {
      case "=":
      case "-=":
      case "+=":
      case "**":
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
    return new ListNode(elements, tokenIndex);
  }

  public static void main(String[] args) throws Exception {
    String fileName = "example.pl";
    String code = "my $var = 42; 1 ? 2 : 3; print \"Hello, World!\\n\";";
    if (args.length >= 2 && args[0].equals("-e")) {
      code = args[1]; // Read the code from the command line parameter
      fileName = "-e";
    }
    Lexer lexer = new Lexer(code);
    List<Token> tokens = lexer.tokenize();
    ErrorMessageUtil errorMessageUtil = new ErrorMessageUtil(fileName, tokens);
    Parser parser = new Parser(errorMessageUtil, tokens);
    Node ast = parser.parse();
    System.out.println(ast);
  }
}

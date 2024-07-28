public class Token {
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


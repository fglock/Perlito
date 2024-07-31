import java.util.ArrayList;
import java.util.List;

/**
 * Utility class for generating error messages with context from a list of tokens.
 */
public class ErrorMessageUtil {
    private String fileName;
    private List<Token> tokens;

    /**
     * Constructs an ErrorMessageUtil with the specified file name and list of tokens.
     *
     * @param fileName the name of the file
     * @param tokens the list of tokens
     */
    public ErrorMessageUtil(String fileName, List<Token> tokens) {
        this.fileName = fileName;
        this.tokens = tokens;
    }

    /**
     * Throws an error message with context from the token list.
     *
     * @param index the index of the token where the error occurred
     * @param message the error message
     * @throws the formatted error message with context
     */
    public String throwError(int index, String message) throws RuntimeException {
      throw new RuntimeException(this.errorMessage(index, message));
    }

    /**
     * Generates an error message with context from the token list.
     *
     * @param index the index of the token where the error occurred
     * @param message the error message
     * @return the formatted error message with context
     */
    public String errorMessage(int index, String message) {
        int line = 1;
        int firstToken = 0;

        // Retrieve the line number by counting newlines up to the specified index
        for (int i = 0; i <= index; i++) {
            if (tokens.get(i).type == TokenType.NEWLINE) {
                line++;
                firstToken = i;
            }
        }

        // Retrieve the string context around the error by collecting tokens near the specified index
        List<String> near = new ArrayList<>();
        for (int i = Math.max(0, index - 3); i <= Math.min(tokens.size() - 1, index + 2); i++) {
            if (tokens.get(i) != null) {
                near.add(tokens.get(i).text);
            }
        }

        // Join the collected tokens into a single string
        String nearString = String.join("", near);

        // Return the formatted error message with the file name, line number, and context
        return message + " at " + fileName + " line " + line + ", near " + errorMessageQuote(nearString) + "\n";
    }

    /**
     * Quotes the specified string for inclusion in an error message.
     * Escapes special characters such as newlines, tabs, and backslashes.
     *
     * @param str the string to quote
     * @return the quoted and escaped string
     */
    private static String errorMessageQuote(String str) {
        StringBuilder escaped = new StringBuilder();
        for (char c : str.toCharArray()) {
            switch (c) {
                case '\n':
                    escaped.append("\\n");
                    break;
                case '\t':
                    escaped.append("\\t");
                    break;
                case '\\':
                    escaped.append("\\\\");
                    break;
                case '\"':
                    escaped.append("\\\"");
                    break;
                default:
                    escaped.append(c);
            }
        }
        return "\"" + escaped.toString() + "\"";
    }

    public static void main(String[] args) {
        // Example usage
        List<Token> tokens = new ArrayList<>();
        tokens.add(new Token(TokenType.IDENTIFIER, "my"));
        tokens.add(new Token(TokenType.IDENTIFIER, "$var"));
        tokens.add(new Token(TokenType.NEWLINE, "\n"));
        tokens.add(new Token(TokenType.OPERATOR, "="));
        tokens.add(new Token(TokenType.IDENTIFIER, "42"));
        tokens.add(new Token(TokenType.IDENTIFIER, ";"));

        // Create an instance of ErrorMessageUtil with the file name and token list
        ErrorMessageUtil errorMessageUtil = new ErrorMessageUtil("example_file.txt", tokens);

        // Generate an error message for a specific token index
        String message = errorMessageUtil.errorMessage(4, "Syntax error");
        System.out.println(message);
    }
}


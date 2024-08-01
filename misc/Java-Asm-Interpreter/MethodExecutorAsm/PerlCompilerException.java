/**
 * PerlCompilerException is a custom exception class used in the Perl compiler.
 * It extends RuntimeException and provides detailed error messages
 * that include the file name, line number and a snippet of code.
 */
public class PerlCompilerException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    private final String errorMessage;

    /**
     * Constructs a new PerlCompilerException using the error message utility
     *
     * @param tokenIndex the index of the token where the error occurred
     * @param message the detail message
     * @param errorMessageUtil the utility for formatting error messages
     */
    public PerlCompilerException(int tokenIndex, String message, ErrorMessageUtil errorMessageUtil) {
        super(message);
        this.errorMessage = errorMessageUtil.errorMessage(tokenIndex, message);
    }

    /**
     * Returns the detailed error message.
     *
     * @return the detailed error message
     */
    @Override
    public String getMessage() {
        return errorMessage;
    }
}


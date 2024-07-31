public class PerlCompilerException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    private final String errorMessage;

    public PerlCompilerException(int tokenIndex, String message, ErrorMessageUtil errorMessageUtil) {
        super(message);
        this.errorMessage = errorMessageUtil.errorMessage(tokenIndex, message);
    }

    @Override
    public String getMessage() {
        return errorMessage;
    }
}

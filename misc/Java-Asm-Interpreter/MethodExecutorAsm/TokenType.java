/**
 * The TokenType enum defines the various types of tokens that can be encountered
 * during the lexical analysis phase of parsing a programming language or text.
 * Each token type represents a specific category of lexical units that the lexer
 * can identify and classify.
 *
 * <p>This enum is used to categorize and manage the different types of tokens,
 * allowing for more organized and efficient handling of lexical analysis within
 * a lexer or parser.</p>
 */
public enum TokenType {
    /**
     * Represents whitespace characters, such as spaces, tabs, and other
     * non-visible characters that separate tokens but do not affect the
     * syntactic structure of the language.
     */
    WHITESPACE,

    /**
     * Represents newline characters, which indicate the end of a line of text.
     * Newlines can be used to separate statements or affect the layout of the code.
     */
    NEWLINE,

    /**
     * Represents identifiers, which are names used to identify variables,
     * functions, classes, and other user-defined entities in the code.
     */
    IDENTIFIER,

    /**
     * Represents numeric literals, which are numbers that appear directly
     * in the code. This includes integers.
     */
    NUMBER,

    /**
     * Represents operators, which are symbols that perform operations on
     * one or more operands. This can include arithmetic operators, logical
     * operators, comparison operators, dots, quotes, and others.
     */
    OPERATOR,

    /**
     * Represents string literals, which include everything else such as
     * Unicode or binary characters. Strings are used to represent text
     * or binary data in the code.
     */
    STRING,

    /**
     * Represents the end-of-file (EOF) marker, which indicates that the lexer
     * has reached the end of the input source. This is used to signal that no
     * more tokens are available.
     */
    EOF
}


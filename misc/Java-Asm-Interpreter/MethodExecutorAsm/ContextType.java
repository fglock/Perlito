/**
 * The ContextType enum defines the various types of contexts that can be encountered
 * in the Perl programming language. Perl has a unique context system that determines
 * how expressions are evaluated and what kind of values are expected or returned.
 *
 * <p>This enum is used to categorize and manage the different contexts in Perl,
 * allowing for more organized and efficient handling of various scenarios within
 * a Perl interpreter or compiler.</p>
 */
public enum ContextType {
    /**
     * Represents a void context in Perl, where no value is expected or returned.
     * This is typically used for functions or operations that do not produce a result.
     * In Perl, this is often seen in statements that are executed for their side effects.
     */
    VOID,

    /**
     * Represents a scalar context in Perl, where a single scalar value is expected
     * or returned. Scalar values in Perl include numbers, strings, and references.
     * This context is used for operations that produce or operate on individual data items.
     */
    SCALAR,

    /**
     * Represents a list context in Perl, where a list of values is expected or returned.
     * This context is used for operations that produce or operate on collections of items,
     * such as arrays or lists. In Perl, list context can affect how functions and operators
     * behave and what they return.
     */
    LIST,

    /**
     * Represents a runtime context in Perl, where the context will be decided at runtime.
     * This means that the context is not known at compile-time and can vary depending on
     * where and how a subroutine or expression is called. For example, a subroutine can be
     * called from different places in different contexts, and the actual context will be
     * determined during the execution of the program.
     */
    RUNTIME
}


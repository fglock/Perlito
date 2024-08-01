/*
 * TODO:
 * - easy wins
 *       - loops
 *       - $_ $@
 *       - ternary operator
 *
 * - harder to implement
 *       - BEGIN block
 *       - eval block, catch error
 *       - test suite
 *
 * - easy, but low impact
 *      - wantarray()
 *      - warn()
 *      - die()
 *      - other builtins
 *
 * - more difficult, and low impact
 *      - caller()
 *      - goto()
 *      - thread
 *      - optimizations
 *
 * - Parser: low-precedence operators not, or, and
 *
 * - cleanup the closure code to only add the lexical variables mentioned in the AST
 *
 * - format error messages and warnings
 *       - compile time: get file position from lexer
 *       - run-time: add annotations to the bytecode
 *
 * - test different Perl data types
 *       - array, hash, string, double, references
 *       - experiment with Perlito runtime
 *
 * - global variables and namespaces
 *       - named subroutine declaration
 *       - Perl classes
 *
 * - local variables
 *     set up the cleanup before RETURN
 *     set up exception handling
 *
 * - add debug information (line numbers)
 *     Label thisLabel = new Label();
 *     ctx.mv.visitLabel(thisLabel);
 *     ctx.mv.visitLineNumber(10, thisLabel); // Associate line number 10 with thisLabel
 *
 * - tests
 *
 * - implement thread-safety - it may need locking when calling ASM
 *
 * - create multiple classes; ensure GC works for these classes
 *
 * - goto, macros - control structures
 *       - test FOR, WHILE
 *
 * - eval string: freeze the ctx.symbolTable at eval string
 *
 * - BEGIN-block
 *
 * - read code from STDIN
 *
 *       // Read input from STDIN
 *       Scanner scanner = new Scanner(System.in);
 *       System.out.println("Enter code:");
 *       String code = scanner.nextLine();
 *       scanner.close();
 *
 */

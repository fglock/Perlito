import java.util.*;

public class Main {

  public static void main(String[] args) {
    try {

      String code =
          ""
              + "my $a = 15 ;"
              + "my $x = $a ;"
              + "print $x ;"
              + "$a = 12 ;"
              + "print $a ;"
              + " ( sub { print @_ } )->(88888) ;"
              + "print $a ;"
              + "do { $a; if (1) { print 123 } elsif (3) { print 345 } else { print 456 } } ;"
              + "return 5;";
      if (args.length >= 2 && args[0].equals("-e")) {
        code = args[1]; // Read the code from the command line parameter
      }

      // Create the compiler context
      EmitterContext ctx =
          new EmitterContext(
              "test.pl", // source filename
              null, // java class name
              new ScopedSymbolTable(), // top-level ctx.symbolTable
              null, // return label
              null, // method visitor
              null, // call context
              false // is boxed
              );
      ctx.symbolTable.enterScope();
      ctx.symbolTable.addVariable("@_"); // argument is local variable 0
      ctx.symbolTable.addVariable("wantarray"); // call context is local variable 1

      // Parse the code
      System.out.println("parse code: " + code);
      System.out.println("  call context " + ctx.contextType);
      Lexer lexer = new Lexer(code);
      List<Token> tokens = lexer.tokenize();
      Parser parser = new Parser(tokens);
      Node ast = parser.parse();
      System.out.println("-- AST:\n" + Parser.getASTString(ast) + "--\n");

      // Create the class
      System.out.println("createClassWithMethod");
      Class<?> generatedClass =
          ASMMethodCreator.createClassWithMethod(
              ctx,
              new String[] {}, // closure variables  { name }
              ast);

      // Convert into a Runtime object
      String newClassName = generatedClass.getName();
      Runtime.anonSubs.put(newClassName, generatedClass);
      Runtime anonSub = Runtime.make_sub(newClassName);
      Runtime result = anonSub.apply(new Runtime(999), ContextType.SCALAR);

      System.out.println("Result of generatedMethod: " + result);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}

/* TODO

  - easy wins
        - loops
        - string

  - harder
        - BEGIN block
        - eval string
        - eval block, catch error

  - Parser: low-precedence operators not, or, and

  - cleanup the closure code to only add the lexical variables mentioned in the AST

  - format error messages and warnings
        - compile time: get file position from lexer
        - run-time: add annotations to the bytecode

  - test different Perl data types
        - array, hash, string, double, references
        - experiment with Perlito runtime

  - global variables and namespaces
        - named subroutine declaration
        - Perl classes

  - local variables
      set up the cleanup before RETURN
      set up exception handling

  - add debug information (line numbers)
      Label thisLabel = new Label();
      ctx.mv.visitLabel(thisLabel);
      ctx.mv.visitLineNumber(10, thisLabel); // Associate line number 10 with thisLabel

  - tests

  - implement thread-safety - it may need locking when calling ASM

  - create multiple classes; ensure GC works for these classes

  - goto, macros - control structures
        - test FOR, WHILE

  - eval string
        freeze the ctx.symbolTable at eval string, we will need it to compile the string later

  - BEGIN-block

  - read code from STDIN

        // Read input from STDIN
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter code:");
        String code = scanner.nextLine();
        scanner.close();

*/

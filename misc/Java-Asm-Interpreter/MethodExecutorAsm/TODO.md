# TODO

## Easy Wins
- Loops
- `$_`, `$@`
- Ternary operator

## Harder to Implement
- `BEGIN` block
- `eval` block, catch error
- Test suite

## Easy, but Low Impact
- `wantarray()`
- `warn()`
- `die()`
- Other builtins

## More Difficult, and Low Impact
- `caller()`
- `goto()`
- Thread
- Optimizations

## Parser
- Low-precedence operators: `not`, `or`, `and`

## Cleanup
- Cleanup the closure code to only add the lexical variables mentioned in the AST

## Format Error Messages and Warnings
- Compile time: get file position from lexer
- Run-time: add annotations to the bytecode

## Test Different Perl Data Types
- Array, hash, string, double, references
- Experiment with Perlito runtime

## Global Variables and Namespaces
- Named subroutine declaration
- Perl classes

## Local Variables
- Set up the cleanup before `RETURN`
- Set up exception handling

## Add Debug Information (Line Numbers)
```java
Label thisLabel = new Label();
ctx.mv.visitLabel(thisLabel);
ctx.mv.visitLineNumber(10, thisLabel); // Associate line number 10 with thisLabel
```

## Tests

## Implement Thread-Safety
- It may need locking when calling ASM

## Create Multiple Classes
- Ensure GC works for these classes

## `goto`, Macros - Control Structures
- Test `FOR`, `WHILE`

## `eval` String
- Freeze the `ctx.symbolTable` at `eval` string

## `BEGIN` Block

## Read Code from STDIN
```java
// Read input from STDIN
Scanner scanner = new Scanner(System.in);
System.out.println("Enter code:");
String code = scanner.nextLine();
scanner.close();
```



Perlito5 Java backend
=====================


Requirements
------------

- building the compiler requires `perl` and `JDK` (for `javac`).
  No extra modules are required.

- minimum Java version is Java 8

  - Perlito5 runtime uses `java.time.ZonedDateTime`,  which was introduced in Java 8.

  - Perlito5 runtime uses `java.nio.file.Files`, which was introduced in Java 7.

  - Java 7 is also required for named groups in regex, like:  `(?<name>X).`


Build using make
----------------

- type:

  ```sh
  $ make
  ```

- alternately:

    - Update the Perl-based compiler `perlito5.pl`

      ```sh
      $ make build-5to5
      ```

    - Compile the compiler into a jar file `perlito5.jar`

      ```sh
      $ make build-5java
      ```

- run a test or two:

  ```
  $ java -jar perlito5.jar -v
  This is Perlito5 9.021, an implementation of the Perl language.

  $ java -jar perlito5.jar -Isrc5/lib t5/unit/array.t
  ok 1 ...
  ```

  - Java specific tests are in `t5/java-specific` directory


Perlito5-Java platform differences
-------------------------------------------

- no timely destruction (DESTROY) (because we use Java memory management)
    - files don't "auto-close" at the end of a block
    - weaken() is a no-op
    - Try::Tiny `finally` doesn't work
    - Object::InsideOut will not cleanup unused objects
    - SelectSaver module doesn't work

- no XS (because we use Java instead of C)
    - many CPAN modules which use C libraries don't work
    - some CPAN modules are already ported, see: src5/lib/Perlito5X/Java/

- some system features are not readily available in Java, such as:
    - file permissions for setuid, setgid, and sticky bit are not implemented
    - some signals are not available in Java.


Perlito5-Java work-in-progress
------------------------------

  - `goto`
      - `goto &code` works, but it doesn't do a tail-call
      - `goto LABEL` - some use patterns work.
        - TODO - test Text::Balanced module
        - TODO - test File::Copy module
      - "computed goto" is not implemented

  - signals are partially implemented
      - `$SIG{__WARN__}` and `$SIG{__DIE__}` are implemented
      - other signals are not yet implemented
      - `$SIG{ALRM}` and `alarm()` not implemented

  - object system
      - method resolution order is not selectable
      - interaction between inheritance and overloading needs more tests
      - interaction between `local` and method cache needs more tests

  - tied variables
      - DESTROY not used, because we use Java memory management

  - overload is partially implemented
      - overload string, number, boolean work
      - binary operators work
      - mutators and assignment work
      - `fallback` needs more tests
      - dereferencing, iterators, filetest not implemented
      - `.` operator not implemented
      - `no overloading` not implemented
      - `overload::constant` not implemented
      - `nomethod` not implemented
      - some optimizations may cause a lower level overload to be called,
        for example: call `0+` instead of `+`, call `""` instead of `.`

  - file handles are partially implemented
      - `open()` scalarref works
      - `<DATA>` works
      - open binary mode vs. open utf8 needs more tests
      - files don't `auto-close`, because we use Java memory management
      - `$|` works, but `STDOUT->autoflush` not implemented

  - `tr()` is partially implemented
      - modifier switches needs some tests

  - subroutines
      - `my sub x {...}` not implemented

  - lvalue $#a and other expressions: substr, ternary, keys, pos
      - needs more tests

  - `local @_` doesn't work yet, because `@_` is special

  - smartmatch `~~` operator partially implemented
    - `when` and `given` not implemented.

  - format() function partially implemented
    - line counter and page-break not implemented
    - sentence break algorithm not implemented

  - incomplete CORE function implementations
      - open()
      - sprintf()
      - pack()
      - unpack()
      - UNIX-specific operators
      - no Unix pipes
        - named pipes are possible using C and JNI
      - See: https://github.com/jnr
      - See: https://github.com/jnr/jnr-posix

  - `reset` and `m??` not implemented

  - clone() is work-in-progress
      - deep clone of references is not implemented
      - clone tied values is not implemented

  - Perl threads not implemented
      - Java threads can be used, with some limitations. See: misc/Java/TestConcurrent.pl and misc/Java/TestThread2.pl


Regex differences
-----------------

  - regex modifiers /ismxgecr work the same as Perl
      - `/xx` works
      - `/ee` works
      - `/r`  works
      - `/g` has problems in certain backtracking zero-length-matches
      - other modifiers are not yet implemented.

  - regex variables `$1`, `$2`, ... and `$&`, `$'`, `` $` `` work; other variables are not yet implemented.

  - capturing in zero-length-match has problems. Failing tests:

    ```
    t5/unit/regex_zero_length_match_capture.t
    t5/unit/regex_zero_length_match_match.t
    t5/unit/regex_zero_length_replace.t
    ```

  - Perl5 allows underscores in named captures. This is not allowed in Java regex.

  - Perl5 allows duplicate names in named captures.

  - TODO - check this error message, this may need to be implemented for compatibility:

    `Unescaped left brace in regex is deprecated, passed through in regex; marked by <-- HERE in m/\G{ <-- HERE / at (eval 2) line 20.`


Eval-string
-----------

Limitations

  - eval compilation is slow; after compilation, the code runs at native speed.

  - eval bytecode is cached - this may leak memory

      - review the ClassLoader for leaks

      - add tests for leaks


Perlito5-Java extensions
===========================

The Perlito5 Java backend doesn't support Perl XS extensions.

Instead of XS, it can use Java classes directly, and there is also a 
`Java` package that connects Perl with Java.


Import a Java class as a Perl package at compile-time, and typed variables
-------------------------------------------------------

Java classes can be added to a Perl script using a special `package` import declaration:

```perl
package Date   { import => "java.util.Date" };
```

The new package can be used as a variable type specification:

```perl
my Date $dt = Date->new();
```

variable types are optional, this also works:

```perl
my $dt = Date->new();
```

- (experimental) as a special case, an empty package works for importing builtin types or primitives (`String`, `Long`, `long`)

```perl
package Integer {}

my Integer $i;
```

Using typed variables
---------------------

```perl
package long {}
my long $j;             # Java variable
my $var;                # Perl variable
$var = $j;              # store Java value in Perl variable
$j = $var;              # get Java value from Perl variable; cast from scalar to long is automatic
```

Typed variables generate efficient, native Java. There are a few restrictions:

- Only `my` variables can be typed.

- Java variables are not captured by Perl closures. This means that a variable declared in a context
will not be seen inside inner subroutine declarations (named or anonymous) and eval blocks. Loops and
conditionals should work fine, because these are not usually implemented as closures.

  - workaround: store the Java value in a Perl variable


Inlining Java code with `Java::inline`
------------

`Java::inline` can be used to add simple Java expressions to a Perl script

  ```perl
  my @arr = Java::inline ' new String[]{ "a", "b" } ';
  ```

- `Java::inline` works in eval-string and in ahead-of-time (pre-compilation) mode

- `Java::inline` can be used inside expressions or in statement position

- Java exceptions can be catched with a Perl eval-block

- example: instantiate an Object with jrunscript

  ```
  $ jrunscript -cp . -l Perl5 
  perl> my $x = Java::inline " new Object() "; say ref($x); say $x; say ($x ? "true" : "false" );
  java.lang.Object
  java.lang.Object@6680f714
  true
  ```

- example: retrieve a Java Class Object

  ```
  $ jrunscript -cp . -l Perl5 
  perl> eval { my $x = Java::inline q{ Class.forName("java.lang.Thread") }; say ref($x); say $x; }
  java.lang.Class
  class java.lang.Thread
  ```

  - only fully qualified class names are supported.

  - support for simple names can be added; the list of classes can be extracted from the ClassLoader

- example: Java method override using Java::inline

  ```perl
  # See: https://github.com/bdevetak/perl2j/tree/master/examples/myapp
  package Date  { import => "java.util.Date" };
  my Date $dateJavaObject =
      Java::inline
          'new Date() {
              public String toString() {
                  return "Hello";    
              }    
          }';
  my $dateString_pObject = $dateJavaObject->toString();   # Hello
  ```

Java fields, methods and constructors
------------------------------------

  - examples:

    ```
    $ jrunscript -cp . -l Perl5 
    perl> my $x = Java::inline q{ (Integer)123 }
    123
    perl> my $x = Java::inline q{ (char)90 }
    Z
    perl> my $x; eval { $x = Java::inline q{ Class.forName("java.lang.Math") } }; say $x->PI
    3.141592653589793
    perl> my $x; eval { $x = Java::inline q{ Class.forName("java.lang.Integer") } }; say $x->MAX_VALUE
    2147483647
    perl> my $x; eval { $x = Java::inline q{ Class.forName("java.lang.Thread") } }; say $x->currentThread()
    Thread[main,5,main]
    perl> $x[10] = eval { Java::inline q{ Class.forName("java.lang.Thread") } }; say $x[10]->currentThread()
    Thread[main,5,main]
    ```

    - TODO add tests for these examples

  - `new` invokes a constructor

  - Simple Java objects (String, Character, Integer, Long, Short, Byte, Double, Boolean) are converted to Perl values.

  - Other Java objects are seen by Perl as "blessed references"

  - Java exceptions can be catched with Perl eval-block

  - Note that explicitly returning a plain `null` from `Java::inline` is a syntax error, because Java doesn't know which class it belongs to:

    ```
    perl> my $x; eval { $x = Java::inline q{ null } }; say $x
    /PlEval3.java:23: error: reference to set is ambiguous
      both method set(String) in PlLvalue and method set(System) in PlLvalue match
    ```

    workaround: cast null to a specific class:

    ```
    perl> my $x; eval { $x = Java::inline q{ (String)null } }; say $x
    ```

`Java` Perl module
-----------------

The `Java` Perl module is meant to emulate the `Java` global object in Nashorn.

- the Nashorn convention for "Using Java from Scripts", see:

  - https://docs.oracle.com/javase/9/scripting/using-java-scripts.htm
  - https://wiki.openjdk.java.net/display/Nashorn/Nashorn+extensions
  - https://github.com/shekhargulati/java8-the-missing-tutorial/blob/master/10-nashorn.md


```
$ jrunscript -cp .:perlito5.jar -l Perl5
perl> push @INC, "src5/lib";
1
perl> use Java

perl> Java->type("java.lang.Thread")
class java.lang.Thread
perl> $Thread = Java->type("java.lang.Thread"); $Thread->new();
Thread[Thread-0,5,main]
perl> $Thread->new( sub { say "123" } )->start()

perl> sub UUID () { Java->type("java.util.UUID") }
perl> UUID->randomUUID()->toString()
9e8aadb3-4d81-41c6-af85-7ab7213a9945

perl> $arr = Java->type("int[]")->new(10)
[I@2cc75074
perl> $arr->length
10
```

- TODO compile Java->type() to static code if the type parameter is a constant


Java extensions in runtime (work in progress)
-------------------------------------------------

  - Syntax
  
    Jython, JRuby, Groovy and Nashorn provide some examples.
  
    Alternately, our own "pre-compile" mode introduced some syntax we could reuse.
  
    See also http://search.cpan.org/dist/Inline-Java/Java.pod
  
    - https://wiki.python.org/jython/UserGuide#interaction-with-java-packages
  

  - TODO add tests - Extensions are allowed in pre-compilation mode and in eval-string mode

  - native Java variables (typed variables)

    ```
    $ jrunscript -cp . -l Perl5 
    perl> package Java::Object { import => 'java.lang.Object' };    # import Java class
    import
    perl> my Java::Object $obj = Java::Object->new();
    java.lang.Object@4fe533ff
    ```

  - Java objects can be assigned to Perl scalar variables, array elements, or hash elements.

  - TODO - Assign from Perl scalar expression to typed variables requires explicit coercion (or unboxing).

    Example: use the `Java` package to create a boxed Java array;
    use a typed variable declaration to create an unboxed Java array:

    ```
    $ jrunscript -cp . -l Perl5 
    perl> push @INC, "src5/lib";
    perl> use Java
    perl> package Java::Array::Of::Int { import => "java.lang.Integer[]" }  
    perl> my Java::Array::Of::Int $aa; $aa = Java->type("Integer[]")->new(10);
    error: incompatible types: PlObject cannot be converted to int[]
    ```

    workaround: assign the expression to a variable, and then assign between variables:

    ```
    perl> package Java::Array::Of::Int { import => "java.lang.Integer[]" }
    perl> my Java::Array::Of::Int $aa; my $bb = Java->type("java.lang.Integer[]")->new(10); $aa = $bb;
    ```

  - TODO - type information is lost for `Byte`, `Character` values - these are converted to `long`, `String`

    - Note that casting an argument to Byte in a method call would not work, because the 
      "argument list" object is a Perl list: it would convert the Byte to a Perl integer.

  - TODO - Array and Hash dereference syntax is not supported
  - TODO - assign to array, assign to hash

    ```
    my $v = Java->type("int[]")->new(10);
    $v->[0] = 123;      # TODO
    @$v = (1..3);       # TODO
    for (@$v) { ... }   # TODO
    for my Integer $x (@$v) { ... }     # TODO
    ```

  - TODO - test varargs call with 0 arguments

  - TODO - syntax for dereferencing scalars (Java objects are stored as references)

    ```
    my $v = Java->type("int[]")->new(10);
    my Integer::Array $i = $$v;     # TODO
    ```

  - TODO - syntax for Java method calls - typed argument lists are work in progress

    - TODO document method resolution order

  - Perl modules using extensions can be precompiled ahead-of-time in `perlito5.jar`, by adding an extra `use` statement in `src5/util/jperl.pl`

    - TODO add tests - interoperation of "ahead-of-time" compiler extensions and "eval-string" extensions


Calling a Perl subroutine from Java
-----------------------------------

- "Java Scripting API"

  - javax.script API registers "Perl5" interpreter
  - See example at `misc/Java/Script.java`

  ```java
  // $ cp misc/Java/Script.java .
  // $ javac -cp .:perlito5.jar Script.java 
  // $ java -cp .:perlito5.jar Script
  
  import javax.script.*;
  
  public class Script {
      public static void main(String[] args) throws Exception {
      
          ScriptEngineManager factory = new ScriptEngineManager();
          ScriptEngine engine = factory.getEngineByName("Perl5");
      
          Object o = engine.eval(" $x = 456; print 123 + $x; \"value was $x\" ");
          System.out.println("result: " + o);
      }
  }
  ```

  - https://docs.oracle.com/javase/8/docs/technotes/guides/scripting/prog_guide/api.html
  - http://download.java.net/java/jdk9/docs/api/javax/script/package-summary.html

- "jrunscript" support

  ```
  $ jrunscript -cp .:perlito5.jar -l Perl5
  perl>
  ```

  ```
  perl> push @INC, "src5/lib";     # initialize @INC to load Perl modules (alternately, set PERL5LIB env variable)
  perl> use Java
  ```

  - enable debugging output: this is the same as setting `-J DEBUG=1` in the command line

    ```
    perl> $Perlito5::Java::DEBUG=1
    ```

- older API (deprecated)

  - using `Main.apply()`

  ```java
  class MyJava {
      public static void main(String[] args) throws Exception {
          Main.init();
          PlObject[] res = Main.apply("main::test", "123");
          for (PlObject s: res) {
              System.out.println("Java result: " + s.toString());
          }
      }
  }
  ```

Importing a Java class into Perl
--------------------------------

```perl
package Sample {
    import => "misc.Java.Sample"
};

package Array::Of::String {
    import => "java.util.ArrayList<String>",
}

my Array::Of::String $x = Array::Of::String->new();
```

Importing Java value types (Typed variables)
--------------------------------------------

Java value types don't need to be imported, but there must exist a Perl package:

```perl
package String    { }
package Long      { }
package Integer   { }
package Boolean   { }
package Short     { }
package Byte      { }
package Double    { }
package Float     { }
package Character { }

my String $x  = "abc";
my Integer $y = 123;
# assigning a Java value to a Perl variable
my $v = Boolean->TRUE;
print "$v\n";   # 1
```

All primitive data type declarations are supported:

```perl
package long      { }
package int       { }
package short     { }
package byte      { }
package float     { }
package double    { }
package boolean   { }
package char      { }
```

Other value types can be imported:

```perl
package Java::Object { import => "java.lang.Object" };
package Java::Date   { import => "java.util.Date" };
package Java::Array::Of::String { import => "java.util.ArrayList<String>" }

my Java::Object $obj = Java::Object->new();

my Java::Array::Of::String $arr = Java::Array::Of::String->new();
$arr->add("123");
$arr->add($p->toString());
```

- native arrays

  TODO tests - Perl arrays can be assigned a native array:
  
  ```perl
  package Byte { };
  @perl_array = JavaCall->toBytes();
  ```
  
  Native array variables can not be created directly.
  As a workaround, use "Java::inline" or "Java->type()".
  
  ```perl
  package String { };
  my @arr = Java::inline ' new String[]{ "a", "b" } ';
  print "arr[0] $arr[0], arr[1] $arr[1]\n";
  ```
  
  ```perl
  my $arr = Java->type("int[]")->new(10);
  ```

- Typed values examples

  - Character

    `Character` values are numeric (not String)

    ```perl
    package Character { };
    my Character $b = 50;
    my Character $b = ord("a");
    my Character $b = $v;   # cast from scalar to Character is automatic (the scalar must have a numeric value)
    ```

    ```perl
    package Character { };
    package String { };
    my Character $b = String->new("a")->charAt(0);
    ```

    but:

    ```perl
    my Character $b = "a";  # wrong: numeric value is zero
    ```

  - Long

    ```perl
    package Long {};
    my Long $b = 100;
    my Long $b = Long->new(100);
    my Long $b = $v;    # cast from scalar to Long is automatic
    ```

  - Float

    ```perl
    package Float {};
    my Float $b = 100.0;
    my Float $b = Float->new(100.0);
    my Float $b = $v;   # cast from scalar to Float is automatic
    ```

- Type propagation problems:

    ```sh
    $ java -jar perlito5.jar -Isrc5/lib  -e ' package float {}; my float $b = 100/3; $x = $b; say $x; '
    error: incompatible types: possible lossy conversion from double to float
            b_103 = (100D / 3D);
    ```sh
    $ java -jar perlito5.jar -Isrc5/lib  -e ' package int {}; my int $b = 100/3; $x = $b; say $x; '
    error: incompatible types: possible lossy conversion from double to int
            b_103 = (100D / 3D);
    ```

    - TODO add a casting operation to the native code emitter in `Perlito5::Java`

- Java methods with type `void` should not be in the last line of a Perl block.
  This is because Perl blocks return the last value, and `void` is not acceptable as a value.

  ```
  perl> package System { import => "java.lang.System" }
  import
  perl> System->gc()
  /PlEval3.java:15: error: 'void' type not allowed here
                      return PerlOp.context(want, System.gc());
  ```

  workaround: add a plain-perl line, such as `return`, `undef`, or `1`.

  ```
  perl> System->gc(); 1
  ```

- TODO more tests: Java variables and expressions as Perl subroutine parameters.

  ```
  $ java -jar perlito5.jar -Isrc5/lib  -e ' package short {}; my short $b = ord("a"); say $b;'
  ```


Thread safety
-------------

Perl global variables are shared between threads.
This includes for example: `$_`, `$a`, `$b`, `$/`, `@INC`, `%SIG`, `$0`, `$1`, `$&`, `$"`.
Perl variable `@_` (the parameter list) is a special case, it behaves internally like a lexical and it may be captured by closures.

`local` stack is shared.

Perlito also has an internal `boolean` stack, which is shared.

Perl lexical variables are not shared between threads.
Variables captured in closures running in different threads
are shared.

Perl operators are not atomic.

- TODO - Cloning the context

  - use the Context (javax.script.ScriptContext in Perlito5ScriptEngine) to store
    per-thread references to PlCx and PlV instances.

  - be aware of circular references when cloning PlV instances.

  - PlStringConstant instances and method caches also need to be cloned

    - TODO - check how 'local &sub' works in threaded perl 

Perlito5 Java development tools
===============================

- Execute a single-line quick test 

  ```bash
  $ perl perlito5.pl -Isrc5/lib -Cjava -e ' sub x { return 123, 5 } my $x = x(); say "$x" ' > Main.java ; javac Main.java ; java Main
  5
  ```

- Rebuild the compiler

  ```bash
  $ make build-5to5 
  ```

`make` rebuilds everything, including the java-eval-string and the nodejs-based compiler

- Perl-Java test suite

  ```bash
  $ make test-5java   # tests the Java-precompile backend
  $ make test-5jar    # tests the Java-eval-string backend (perlito5.jar)
  $ make test         # tests the nodejs backend
  ```

  `make test` should pass everything.

  `make test-5java` passes a few tests

- Syntax tree

  You may find useful when debugging,

  ```bash
  $ perl perlito5.pl -Isrc5/lib -Cast-json -e ' … '
  $ perl perlito5.pl -Isrc5/lib -Cast-perl5 -e ' … '
  ```

to see the internal representation

- Other

use `make clean` to get rid of all those .class files


Perlito5 Java backend TODO list
===============================


CPAN distribution
-----------------

This documentation should be copied to file Perlito5::Java, in the CPAN distribution.


Java-specific command line options
----------------------------------

  - specify main Class name (currently "Main")
  
  - have a way to port a simple .pm to a .jar

  - have a way to port a simple .pm to a .java (without a main function)
  
      - specify input arguments

      - specify context (list, scalar, void)

      - specify what we want to return: PlObject vs. array of strings, etc
  
  - `-J DEBUG=1` - enable debugging output

    ```bash
    $ java -jar perlito5.jar -I src5/lib -J DEBUG=1 -e ' say 123 '
    ```

Java distribution
----------------------

  - upload the perlito5 jar to a Maven repository

      - See: https://maven.apache.org/guides/mini/guide-central-repository-upload.html

  - add more options to Makefile

      - build stand-alone, precompiled "secure" script without eval-string / without "perlito5-lib"

        ```sh
        $ java -jar perlito5.jar -I src5/lib --nojava_eval -Cjava t5/unit/array.t > test.java
        $ javac test.java
        $ java Main
        ```

      - build android script

      - integrate with Perlito5 in CPAN

Precompile modules
------------------

      - TODO - transparently load `pmc` files in `require` and `use`.

      - `pmc` files could be plain `jar` files.

  - add `src5/lib` Perl files to `perlito5.jar`, instead of setting `-I src5/lib` or `PERL5LIB=src5/lib`.

    - precompile the modules in `lib`


BEGIN blocks
------------

      - Loops containing: BEGIN blocks, `use` statements, or named subroutines.

          - lexical variables inside loops may not behave properly if they are captured at compile-time.

      - some data structures created by BEGIN need more work for proper serialization to AST:

          - some types of aliased values, like:  `*name2 = *name1{IO}`

          - lexical variables are not shared between closures created in BEGIN blocks

      - FIXED - bug capturing BEGIN variables in eval-string:

        ```
        $ java -jar perlito5.jar -I src5/lib -Cperl5 -e ' my @v; BEGIN { @v = (123); sub x { @v }; eval " sub yy { \@v }  " } x; yy; '
        *main::x = do {;
            sub {;
                @Perlito5::BEGIN::_100_v
            }
        };
        *main::yy = do {;
            sub {;
                @v      # <--- this should be @Perlito5::BEGIN::_100_v
            }
        };
        ```

      - bug capturing BEGIN variables in eval-string:

        Variables created during runtime are not captured by BEGIN blocks in eval-string.

        In this example, the named sub declaration is compiled at BEGIN time in eval:

        ```
        $ java -jar perlito5.jar -I src5/lib -e ' my @v = (123); sub yy; eval " sub yy { \@v }  "; print yy(); '
        [empty string; expected: 123]
        ```

        Anonymous subs are not affected by this bug, because anon subs are not BEGIN time.


  - runtime error messages sometimes do not include the line number in the Perl code

      - also caller() is only partially implemented

      - BEGIN line numbers show the line number at the time of eval - the line number is relative to the start of the BEGIN block


JVM and Java compiler related
------------------------------

- Java 8 triggers this problem:

      - http://stackoverflow.com/questions/30707387/troubleshoot-slow-compilation
      - http://stackoverflow.com/questions/34223249/slow-compilation-with-jooq-3-6-plain-sql-and-the-javac-compiler

      - "The workaround is to compile at Java 7-compatibility level: javac , or just to use simpler constructions.
      - "the workaround is to introduce local variables when there are nested generic method calls that use generic type inference


- detect and fix "unreachable code"

      - See: `t5/cmd/switch.t` line 65


- Workaround JVM bytecode size limit

  According to the Java Virtual Machine specification,
  the bytecode of a method must not be bigger than 65536 bytes:

  - See: `$Perlito5::CODE_TOO_LARGE` in `/src5`

  - Test.java:2309: error: code too large

  - when compiling `misc/Java/code_too_large.pl`

  - current workaround: insert a closure every 100s of lines in a block:

    ```perl
    code...
    return (sub {
            code ...
            return (sub {
                    code ...
                }->() )
        }->() )
    ```

XS support using libperl.so
-----------------------------------

- Other implementations

  - Jperl
  
    - http://www.drdobbs.com/jvm/jperl-accessing-perl-from-java/184410852
  
  - BSFPerl
  
    - http://bsfperl.sourceforge.net/tutorial/
  
    - https://sourceforge.net/p/bsfperl/discussion/307607/


Perlito5-Java extensibility in "pre-compile" mode
------------------------------------

This documentation should be copied to file Perlito5::Java, in the CPAN distribution.


- Java import

  ```perl
  package Sample {
      import => "misc.Java.Sample"
  };
  ```

  - the type specification can also contain a type argument

  ```
  package Array::Of::String {
      import => "java.util.ArrayList<java.lang.String>",
  }
  ```

  ```
  package Array::Of::String {
      import => "java.lang.String[]",
  }
  ```


  - TODO: what happens when a class is imported again
        - for example, import `Int` or `Byte` again

  - TODO: test that Perl modules can import Java classes
        - only tested in `main` program

- Typed variables

    `my TYPE VARLIST`
        - See also: http://perldoc.perl.org/functions/my.html

    - no `global` typed variables (only `my` variables)

    - only scalar variables (no hash, array, code)

  - Note:
        - parameters to native calls are evaluated in scalar context
        - untyped variables are passed by reference - that is, v_x instead of v_x.get()
        - wantarray() context is not passed to native calls

    ```bash
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package Sample { import => "misc.Java.Sample" }; my $x = Sample->new(); ' > Test.java ; javac Test.java
    ```

    ```
    my $p_put = Sample->new();
    my $p_put = new Sample();
    ```

  - TODO - list assignment

    `my Integer ($i, $j) = (123, 456);`

    - creates a boxed Java variable           (DONE)

  `"$x"      # Sample<0x1234567>`

    $x is a Perl variable that contains a native `Sample`; it behaves like a Perl object

  `my $x = $p_put;`

    - puts the boxed object into a Perl scalar  (DONE)

  `my Sample $put = Sample->new();`

    - creates a native Java variable          (DONE)

  `my Int $val = Sample->VAL;`

    - "method call without parenthesis"

    read a class or instance variable

  `my $x = $put;`

    - puts the unboxed object into a Perl scalar  (DONE)

  `my $x = Sample->new()`

    - stores the boxed pSample object in a Perl scalar (DONE)

    ```
    package Int { import => 'java.lang.Integer' };
    my Int $x = 3;          # $x is a Java Integer (not a PlScalar)  (TODO: test)
    ```

  - TODO: capture typed variables in closures

    maybe TODO: allow typed variables in parameter list;
    but they would probably lose the type information

  - TODO test: call Perl subroutines with native parameters

  - TODO test: call Java methods with Perl parameters

    ```perl
    Sample->new(10);                # native int
    Sample->new("abc");             # native String
    Sample->new($v);                # cast to the declared parameter type
    Sample->new(0 + $v);            # cast to int
    Sample->new(0.0 + $v);          # cast to double
    Sample->new("" . $v);           # cast to string
    ```

  - Method chaining:

    ```
    my $global_queue = ConcurrentLinkedQueue::Of::String->new();
    my ConcurrentLinkedQueue::Of::String $queue = $global_queue;
    my $x = $queue->poll();

    my $x = $global_queue->poll();    # uses reflection to find the method
    ```

  - TODO more tests: cast perl object to java object

    ```
    my Result $java_obj_result = $scan_result;
    ```

  - Array-deref:

    `@$native` will retrieve an iterator and produce a Perl list
    (TODO)

    and "keys %$native" can be written like:

    ```
    @{ $native->entrySet() }
    @{ $native->keySet() }
    @{ $native->values() }
    (TODO)
    ```

  - Array-deref in boolean context:
        
    Automatic call to ->hasNext on iterator reference in
    boolean context. The idea is that instead
    
    ```perl
    while ($iterator->hasNext()) { ... }
    ```

    we can type:

    ```perl
    for (@$bar) { ... }
    ```

    First, the @$bar dereferencing of native java list would obtain
    an iterator reference and then due to having iterator reference in
    boolean context, the call to hasNext() would autmatically happen.
    That way we will be able to write idiomatic Perl loops on
    native java Lists

    TODO - this is partially implemented; add tests.     

  - Native values in Perl expressions:

    in this case, we can just assign the value to PlLvalue
    because PlLvalue knows what to do with each type
    this also takes care of `$x` and `0+$x`
    (TODO)
    
    PlLvalue.set() is overloaded - Array, Hash, int, string, boolean, ...

    src5/lib/Perlito5/Java/Runtime.pm 1463

    (TODO) comparison and logical operators with Java variables:

    ```
    if ($typed == $typed) ...
    if ($typed eq $typed) ...
    if ($typed || $typed) ...
    if ($typed && $typed) ...
    ```

- interoperation between native expressions and perl expressions

    method calls on typed variables call Java methods,
    method calls on untyped variables call Perl methods

    subroutines are always `Perl`

    Class method calls on imported classes are `Java`,
    all other Class method calls are `Perl`.

    Parameters to Java method calls should be converted to Java automatically.  (TODO: test, examples)

    storing a Java object into a Perl scalar converts the value to a `Perl` object (blessed reference);
    Java `int`, `String`, `double` are converted to Perl values.

    using Java objects in other types of expressions than scalar assignment is not well defined. (TODO: test, examples)

    using Perl objects in Java code is not well defined. (TODO: test, examples)

    storing a Java object into a typed variable keeps the Java object as-is.

  - test case:

    ```bash
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package my::Sample { import => "misc.Java.Sample" }; my $x = my::Sample->new(); say "ref: ", ref($x), " x ", $x; my @arr = (1,2,5); say ref(\@arr); $x = \@arr; say ref($x); my my::Sample $z = my::Sample->new(); $x = $z; 
    ```

  - (maybe deprecated) everything at the right side of `...->to_JavaThing()->...` is native-call


- native expressions

    ```perl
    $x++;         # autoincrement
    $x = $x + 1;  # assignment
    print $x;     # print
    ```

  - Note that native variables must be initialized:

    ```
    java -jar perlito5.jar -Isrc5/lib  -e ' package Integer {}; my Integer $x; $x ++ ' 
    error: variable x_103 might not have been initialized
            x_103++;
    ```

  - test case:

    ```perl
    package Integer {}
    my Integer $count;
    my Integer $i = 0;
    while ( $i < 400 ) {
        my Integer $j = 0;
        while ( $j < 400 ) {
            my Integer $k = 0;
            while ( $k < 400 ) {
                $k = $k + 1;
                $count = $count + 1;
            }
            $j = $j + 1;
        }
        $i = $i + 1;
    }
    my $c = $count;
    print "done $c\n";
    ```


Value types
---------------

- Native array variables:

  ```perl
  # import a native array Class
  #
  #  package Java::Array::Of::String {
  #       import => "java.lang.String[]",
  #  }
  ```

  - not implemented:

    - Conversion from Perl scalar to native array is not implemented.

    ```perl
    # Possible implementation for creating native array variables
    #
    # my byte @bytes;
    # $#bytes = 9;      # indexes 0..9 (10 elements)
    ```

    ```perl
    # Not implemented: assign native array to Perl scalar
    #   (assign to Perl array works, see above)
    #
    # my $arr3 = Java::inline ' new String[]{ "a", "b" } ';
    # print "arr3[1] $arr3->[0]\n";
    # print "arr3[1] $arr3->[1]\n";
    ```

  - (this syntax is maybe deprecated) Conversion from Perl scalar to native array is not implemented.

    ```perl
    # TODO - Not implemented: Perl scalar to native array
    # using auto-generated methods
    #
    # package String { };
    # $arrayref->toStringArray()
    #
    # package int { };
    # $arrayref->to_intArray()
    #
    # package Java::Date   { import => "java.util.Date" };
    # $arrayref->to_JavaDateArray()
    ```

  - See: toArray(T[] a) in https://docs.oracle.com/javase/7/docs/api/java/util/ArrayList.html

    in Perl:

        char::Array, boolean::Array, float::Array, double::Array, long::Array, short::Array, int::Array, byte::Array

    ```perl
    # alternate implementation:
    #
    # package String { };
    # my String::Array = String::Array->new(@array)
    ```
 
    ```bash
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package byte::Array { type => 'byte[]' } my byte::Array $x = byte::Array->new("A","B","C");'
    ```

    this should return:

    ```java
    byte[] v_x = new byte[3]; // alocate 3 bytes - the native arrays in java are not dynamic
    byte[0] ='A';
    byte[1] = 'B';
    byte[2] = 'C';
    ```

    or:

    ```java
    byte[] v_x = new byte[] { "A", "B", "C" };
    ```

  - alternately:

    ```perl
    package my_chars { type => 'char[]' }
    ```

  - Investigate adding support for plain "Object" arguments.
        See http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html arguments


- mixing Java containers, Perl, and Java::inline
    See: misc/Java/TypedIterator.pl

  ```perl
  package Iterator::Of::String {
      import => "java.util.Iterator<String>",
  };
  package ArrayList::Of::String {
     import => "java.util.ArrayList<String>",
  }
  
  sub foo {
      my $x;
      Java::inline '
          ArrayList<String> listA = new ArrayList<String>();
          listA.add("element 1");
          listA.add("element 2");
          listA.add("element 3");
      ';
      $x = Java::inline "listA";
      return $x;
  }
  
  my $bar = foo();
  my ArrayList::Of::String $arr = $bar;
  my Iterator::Of::String $iterator = $arr->iterator();
  while($iterator->hasNext()) {
    my $element = $iterator->next();
    say $element;
  }
  ```

- autobox as-needed

  - add tests

  - runtime methods should accept String, int, double, boolean types
    and maybe other types of number (byte, ...)

  - Array and Hash should accept other types of containers

    - String accepts char in constructor (DONE)
    - Hash accepts String for index (DONE)
    - Array accepts int for index   (DONE)


(experimental) Extending a Java class with Perl
--------------------------------

Extending a Java class with Perl is very experimental, the API is going to change.

These extensions are experimental and may be deprecated:

  - (experimental) an `extends` specification works for adding methods to an existing class

    - TODO - (experimental) test syntax for creating new Java subclass ("extends" and "implements")

  - (experimental) an `implements` specification works for adding methods to an existing interface

  - (experimental) a `header` specification works for creating a Java package

```perl
# create a Java package
package header { java_path => 'org.perlito.udfs' };

# import the original Java class
package Java::Date   { import => "java.util.Date" };

# create and import the extended class
package My::Date {
    extends => 'Java::Date',
    decl => [ "public", "final" ],              # public final class
    'Java::inline' => " // ... Java code ... \n",
    methods => [
        toString => {
            decl => [ "public" ],               # public method
            args => [],                         # no arguments
            return => "String",                 # returns String
            code => "main::my_date_string",     # implemented in Perl, see below
        },
    ],
}

package main;

# Perl implementation for My::Date->toString()
sub my_date_string {
    my $self = shift;
    print "date_string: self is $self\n";
    return "Hello";
}

my Java::Date $j_date = Java::Date->new();
my $s1 = $j_date->toString();   # original class
my My::Date $date = My::Date->new();
my $s2 = $date->toString();     # extended class

print $s1, " ", $s2, "\n";   # prints date and "Hello"
```


Autovivification of aliased parameters
--------------------------------------

  - (DONE)

    ```bash
    $ perl -e ' use Data::Dumper; my %z; my $s; my @w; sub x {$_[0] = 3} x($z{d}, $s, $w[3]); print Dumper [\%z, $s, \@w] '
    $VAR1 = [
          {
            'd' => 3
          },
          undef,
          []
        ];
    $ perl -e ' use Data::Dumper; my %z; my $s; my @w; sub x {$_[1] = 3} x($z{d}, $s, $w[3]); print Dumper [\%z, $s, \@w] '
    $VAR1 = [
          {},
          3,
          []
        ];
    $ perl -e ' use Data::Dumper; my %z; my $s; my @w; sub x {$_[2] = 3} x($z{d}, $s, $w[3]); print Dumper [\%z, $s, \@w] '
    $VAR1 = [
          {},
          undef,
          [
            undef,
            undef,
            undef,
            3
          ]
        ];
    ```

Slices
------

    DONE $scalar = delete @hash{qw(foo bar)}; # $scalar is 22
    DONE @array  = delete @hash{qw(foo baz)}; # @array  is (undef,33)
    url: http://perldoc.perl.org/functions/delete.html

    DONE $scalar = @hash{qw(foo bar)}; # $scalar is 22
    DONE @array  = @hash{qw(foo baz)}; # @array  is (undef,33)

    DONE print "@hash{qw(foo bar)}" # should print: 11 22

    DONE @hash{qw(foo bar)} = $scalar; # %hash{foo} is $scalar && %hash{bar} is undef
    DONE @hash{qw(foo baz)} = @array; # %hash{foo} is @array[0] && %hash{bar} is @array[1]

  - TEST

    ```perl
    %hash = (foo => 11, bar => 22, baz => 33); $scalar = delete @hash{qw(foo bar)}; print "$scalar\n"
    %hash = (foo => 11, bar => 22, baz => 33); @array = delete @hash{qw(foo bar)}; print "@array\n"

    %hash = (foo => 11, bar => 22, baz => 33); $scalar = @hash{qw(foo bar)}; use feature 'say'; say $scalar
    %hash = (foo => 11, bar => 22, baz => 33); @array = @hash{qw(foo bar)}; use feature 'say'; say "@array"

    %hash = (foo => 11, bar => 22, baz => 33); print "@hash{qw(foo bar)}"

    my $scalar = 11; @hash{qw(foo bar)} = $scalar; use feature "say"; say $hash{foo}
    my @array = (11, 22); @hash{qw(foo bar)} = @array; use feature "say"; say "@array"
    ```

Variables
---------

  - delete local EXPR

  - subroutine lookups could also be "our"-like (also method lookups)


Overflow from long to double
---------------------------

  - partially implemented - needs more work, tests

  - Note: integer operations may have problems with type erasure

  - example:

        $i + subr();  # subr() returns pObject instead of pInt

  - this needs more tests


Tail-call
---------

    caller sends a parameter to enable tail-call
    callee pushes the call to a stack
    caller pops from the stack and call the closures in order

- tailcalls

    same-subroutine tailcalls could execute a `redo` in the current subroutine.

Missing features, or partially implemented, or untested
-------------------------------------------------------

- Object-related

  - `UNIVERSAL::`

        can
        isa
        DOES

  - (DONE) Scalar::blessed

  - TODO tests - unit tests (work in progress)

  - TODO tests - tests for modules in `src5/lib/Perlito5X` and `src5/lib/Perlito5X/Java`

  - TODO tests - invalidate method cache when subroutine changes or @INC changes

- Perl features

  - overload

    - TODO - remove optimizations when one of the operators is overloaded

  - tie()

    - TODO tie filehandle

      ```
      $ java -jar perlito5.jar -Isrc5/lib  -e ' tie *FH, "NewHandle";'
      Can't locate object method "TIEHANDLE" via package "NewHandle" (perhaps you forgot to load "NewHandle"?)
      ```

    - how does Perl behave when tieing a SCALAR to a Handle?
    - lazy lookup: possibly incomplete impl for proxy objects, this needs more tests
    - TODO test - tie() in BEGIN

  - exceptions

        `goto`, `last`, `redo`

        Note: remember to cleanup the stacks (boolean, local).

  - `continue` blocks

  - `dualvars`

        for string-to-number optimization

  - unicode-strings vs. byte-strings

  - subroutine signatures

  - return (list)

  - assignment to splice

  - (DONE) sprintf

        http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...)

        http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html

  - pack

  - unpack

  - typeglob and symbol table operations

      - TODO - add tests (See: Symbol.pm)

        ```bash
        $ java -jar perlito5.jar -I src5/lib  -e ' use Data::Dumper; %This::xyz = (); $This::X::y = 0; $This::vvv = 234; %x = %This::; print Dumper (\%x); print Dumper \%This::X::; print Dumper \%::; '
        $VAR1 = {
                'X::' => *This::X::,
                'vvv' => *This::vvv,
                'xyz' => *This::xyz,
            };
        $VAR1 = {
                'y' => *This::X::y,
            };
        ```

      - TODO - < $This::X::y > is not creating a "namespace"; but < $This::X::y = 0 > does work.


  - caller

      - TODO - add tests (See: perldoc -f caller)

        ```bash
        $ perl -e ' sub x { print "in " . __PACKAGE__ . "\n"; v() }  x(); { package X; *x = \&main::x; x() } sub v { $v = caller(); print "called from $v\n" } '
        in main
        called from main
        in main
        called from main
        ```

      - TODO - line number is off by 1

      - TODO - anonymous subroutines and eval string are not shown in the call stack

        ```bash
        $ java -jar perlito5.jar -I src5/lib -e ' sub x { print "@{[ caller($_) ]}\n" for 0..3; } sub yy { eval "x()"; }; ( sub { yy() } )->(); '
        main -e 1 main::x
        main -e 1 main::yy

        $ perl -e ' sub x { print "@{[ caller($_) ]}\n" for 0..3; } sub yy { eval "x()"; }; ( sub { yy() } )->(); '
        main (eval 1) 1 main::x 1    0
        main -e 1 (eval) 0  x()  256
        main -e 1 main::yy 1    0
        main -e 1 main::__ANON__ 1    256
        ```

      - namespaces need more work:

        ```bash
        $ perl -e ' package AAA; sub x { my $v = caller; print "caller: $v\n"; print "$_: @{[ caller($_) ]}\n" for 0..3; }; package BBB; sub yy {AAA::x(); }; package CCC; sub cc { BBB::yy() }; cc(); '
        caller: BBB
        0: BBB -e 1 AAA::x 1    0  
        1: CCC -e 1 BBB::yy 1    0  
        2: CCC -e 1 CCC::cc 1    256  
        3: 

        $ java -jar perlito5.jar -I src5/lib -I . -e ' package AAA; sub x { my $v = caller; print "caller: $v\n"; print "$_: @{[ caller($_) ]}\n" for 0..3; }; package BBB; sub yy {AAA::x(); }; package CCC; sub cc { BBB::yy() }; cc(); '
        caller: BBB
        0: BBB -e 1 AAA::x
        1: CCC -e 1 BBB::yy
        2: main -e 1 CCC::cc
        3:    
        ```

  - `__DATA__` sections

        `%Perlito5::DATA_SECTION` contains the `__DATA__` for each package

  - add test:

        `print " ${__PACKAGE__} \n"`

- BUG - auto modification is not lazy:

  ```
  $ perl -e ' $c{$_} //= scalar keys %c for qw(a b c a b d); use Data::Dumper; print Dumper \%c '
  $VAR1 = {
            'b' => 2,
            'a' => 1,
            'd' => 4,
            'c' => 3
          };
  ```

  ```
  $ jrunscript -cp .:perlito5.jar -l Perl5
  perl> $c{$_} //= scalar keys %c for qw(a b c a b d); use Data::Dumper; print Dumper \%c
  $VAR1 = {
          'a' => 0,
          'b' => 1,
          'c' => 2,
          'd' => 3,
      };
  ```

- Add tests

  - `NaN`, `Inf`, `-0`

  - `@perl_array = java_native[]`

        supported types: byte[], int[], and imported_class[]

        not implemented: long[], String[], Double[], char[]

- Add tests

  ```
  $ java -jar perlito5.jar -I src5/lib/ -e ' my $v = ({foo => "bar"})[0]{foo}; print "$v\n" '
  Not an ARRAY at -e
  ```

  ```
  $ nodejs perlito5.js     -I src5/lib/ -e ' my $v = ({foo => "bar"})[0]{foo}; print "$v\n" '
  bar
  ```

- Add tests

  ```
  $ java -jar perlito5.jar -J DEBUG=1 -Isrc5/lib -e ' sub x { eval " \$_[0] + 1 " } print x(10), "\n" '
  11
  ```

Regex
-----

  - Quotemeta: \Q

  - Modifiers are not serialized yet (DONE - add tests)

    ```bash
    $ perl -e ' my $user_agent_regexp = "123";  my $regexp = qr/$user_agent_regexp/x; print $regexp; '
    (?^x:123)
    ```

  - but a qr// on an existing qr// behaves differently:

    ```bash
    $ perl -le ' my $x = qr/ (\w) /; my $z = qr/$x/x; print $z '
    (?^: (\w) )
    
    $ perl -le ' my $x = qr/ (\w) /; my $z = qr/before $x after/x; print $z '
    (?^x:before (?^: (\w) ) after)
    ```

See also:

  - "Replace Rhino regexp implementation with java.util.regex"
    - https://bugzilla.mozilla.org/show_bug.cgi?id=390659

  - DFA
    - http://www.brics.dk/automaton/index.html

  - Mini-language with fast regex, Artistic License
    - http://jint.sourceforge.net

  - regex reference:
    - http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html

  - http://www.regular-expressions.info/reference.html

  - Discussion about alternative implementations:
    - http://stackoverflow.com/questions/415580/regex-named-groups-in-java

  - Extended Java java.util.regex, GPL
    - supports Recursive and Conditional Regular Expressions, Capture Trees and Plugins.
    - https://github.com/florianingerl/com.florianingerl.util.regex

  - extensive comments about problems in Java regex
    - https://stackoverflow.com/questions/5767627/how-to-add-features-missing-from-the-java-regex-implementation/5771326#5771326

  - "Perldoop2, a Big Data-oriented Perl-Java source-to-source compiler"
    - https://github.com/citiususc/Perldoop2
    - uses JRegex: http://jregex.sourceforge.net/


Storable
-------

See:

  - Storable for python
    - https://github.com/CowboyTim/python-storable

  - related, pure perl module
    - http://search.cpan.org/~ilyaz/FreezeThaw-0.5001/

Memory management
-----------------

  - Experiment implementing DESTROY with java.lang.ref.Cleaner

    - https://docs.oracle.com/javase/9/docs/api/java/lang/ref/Cleaner.html

    - http://www.enyo.de/fw/notes/java-finalization-revisited.html

Threads
-------

  - See: http://perldoc.perl.org/perlmod.html#Making-your-module-threadsafe
        CLONE, CLONE_SKIP

  - global variables should be cloned in threads,
    use this to get the thread id: Thread.currentThread().getId()

  - Examples:

    ```
    misc/Java/TestConcurrent.pl
    misc/Java/TestThread.pl
    misc/Java/TestThread2.pl    # atomic
    ```

  - pass the threadid as a parameter to all closures;

    closure can use that thread id to get/set the value from globals hash.

  - TODO - per-thread: "local" stack, "boolean" stack, `regex_result`

Optimizations
-------------

  - inline the block in `map`, `grep`, `sort`

  - do-block and eval-block in void-context don't need a subroutine wrapper

  - don't pre-expand ranges in loops (DONE)

  - replace regex with index

  - use `continue` and `break` when possible (in place of Perl `next`, `last`)

  - identify variables that don't need a true `lvalue` container;
    store in a `PerlObject` instead of `PerlLvalue`,
    this is one less level of indirection.

  - investigate performance of "proxy" lvalues;
    when taking an lvalue out of an array or hash, return a proxy
    with a reference to the container and index.

  - investigate using Lambdas:
    https://stackoverflow.com/questions/26257266/are-java-8-lambdas-compiled-as-inner-classes-methods-or-something-else

  - investigate bundling together the calls to `Perlito5::Grammar::Block::eval_begin_block()` to reduce
    the java compiler initialization overhead.

    `eval_begin_block()` is currently called for each named subroutine definition.

Possible workarounds for slow compilation:

  - Java `ASM`
      - TODO: prototype eval-string with ASM
      - See: `misc/Java_asm/`

  - write a tiny interpreter for simple expressions

  - preload modules in `src5/util/jperl.pl`



Bootstrapping using `perlito5-lib.jar`
-------------------------------------

- bootstrapping is not possible with perlito5.jar, because it is built without the grammar modules.

  ```sh
  $ perl perlito5.pl --bootstrapping -Isrc5/lib -Cjava src5/util/perlito5.pl > Main.java

  $ javac -J-Xms2000m -J-Xmx2000m -J-Xss1000m  Main.java
  # errors - TODO - fixme
  #   this is because the compiler uses eval-string
  #   try: --nojava_eval

  # test the bootstrapping
  $ java Main --bootstrapping -Isrc5/lib -Cjava src5/util/perlito5.pl > Main2.java
  $ diff Main.java Main2.java
  [ no differences ]
  ```

- Using the perlito5-lib.jar file

  - TODO - explain this

    ```java
    import org.perlito.Perlito5.*;
    ```


Modules
-------

  - ported modules are in `src5/lib/Perlito5X/Java` and `src5/lib/Perlito5X`

  - there is a port of JSON.pm - it is pure-perl and slow. It would be nice to have a native-java version.

    update: `src5/lib/Perlito5X/Java/JSON.pm` uses `javax.json.stream.JsonParser`
    but this JsonParser class doesn't seem to be installed by default.

  - TODO: DBI.pm


Attributes
----------

  ```
  $ perl -e ' sub MODIFY_CODE_ATTRIBUTES { print "called with (@_)\n"; return }  sub xxx :Integer { print 123 } '
  called with (main CODE(0x7fb99a02db18) Integer)
  ```



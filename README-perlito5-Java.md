
Perlito5 Java backend
=====================


Requirements
------------

- minimum Java version is Java 8

  - Perlito5 runtime uses `java.time.ZonedDateTime`,  which was introduced in Java 8.

  - Perlito5 runtime uses `java.nio.file.Files`, which was introduced in Java 7.

  - Java 7 is also required for named groups in regex, like:  `(?<name>X).`

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

Perlito5-Java work-in-progress
------------------------------

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

  - precompile modules

      - TODO - transparently load `pmc` files in `require` and `use`.

      - `pmc` files could be plain `jar` files.

  - bootstrapping using `perlito5-lib.jar`

      - bootstrapping is not possible with perlito5.jar, because it is built without the grammar modules.

        ```sh
        $ perl perlito5.pl --bootstrapping -Isrc5/lib -Cjava src5/util/perlito5.pl > Main.java

        $ javac -J-Xms2000m -J-Xmx2000m -J-Xss2000m -source 7 Main.java
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


  - Java 8 triggers this problem:

      - http://stackoverflow.com/questions/30707387/troubleshoot-slow-compilation
      - http://stackoverflow.com/questions/34223249/slow-compilation-with-jooq-3-6-plain-sql-and-the-javac-compiler

      - "The workaround is to compile at Java 7-compatibility level: javac -source 7, or just to use simpler constructions.
      - "the workaround is to introduce local variables when there are nested generic method calls that use generic type inference


  - detect and fix "unreachable code"

      - See: `t5/cmd/switch.t` line 65


  - BEGIN blocks

      - Loops containing: BEGIN blocks, `use` statements, or named subroutines.

          - lexical variables inside loops may not behave properly if they are captured at compile-time.

      - some data structures created by BEGIN need more work for proper serialization to AST:

          - some types of aliased values, like:  `*name2 = *name1{IO}`

          - lexical variables are not shared between closures created in BEGIN blocks

      - bug capturing BEGIN variables in eval-string:

        ```
        $ time java -jar perlito5.jar -I src5/lib -Cperl5 -e ' my @v; BEGIN { @v = (123); sub x { @v }; eval " sub yy { \@v }  " } x; yy; '
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

  - runtime error messages sometimes do not include the line number in the Perl code

      - also caller() is only partially implemented

      - BEGIN line numbers show the line number at the time of eval - the line number is relative to the start of the BEGIN block

  - `goto`
      - `goto &code` works, but it doesn't do a tail-call
      - `goto LABEL` - some use patterns work.
        - TODO - test Text::Balanced module
        - TODO - test File::Copy module
      - "computed goto" is not implemented

  - signals are partially implemented
      - `$SIG{__WARN__}` and `$SIG{__DIE__}` are implemented
      - other signals are not yet implemented.

  - object system is partially implemented
      - method resolution order is not selectable
      - interaction between inheritance and overloading need more tests
      - interaction between "local" and method cache more tests

  - tied variables are partially implemented
      - DESTROY not used, because we use Java memory management
      - lazy lookup: possibly incomplete impl for proxy objects, this needs more tests
      - TODO tie filehandle

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

  - file handles are partially implemented
      - open scalarref works
      - `<DATA>` works
      - open binary mode vs. open utf8 needs more tests
      - files don't `auto-close`
      - `$|` and `STDOUT->autoflush` not implemented

  - `tr()` is partially implemented
      - modifier switches needs some tests

  - subroutines
      - `my sub x {...}` not implemented

  - lvalue $#a and other expressions: substr, ternary, keys, pos
      - needs more tests

  - `local @_` doesn't work yet, because `@_` is special

  - smartmatch `~~` operator not implemented; also `when` and `given` not implemented.

  - incomplete CORE function implementations
      - open()
      - sprintf()
      - pack()
      - unpack()
      - format()
      - UNIX-specific operators
      - no Unix pipes
        - named pipes are possible using C and JNI
      - See: https://github.com/jnr
      - See: https://github.com/jnr/jnr-posix

  - clone() is work-in-progress
      - deep clone of references is not implemented
      - clone tied values is not implemented

  - Perl threads not implemented
      - Java threads can be used, with some limitations. See: misc/Java/TestConcurrent.pl and misc/Java/TestThread2.pl


Regex differences
-----------------

  - regex modifiers /ismxgec work the same as Perl; other modifiers are not yet implemented.
      - /xx works
      - /ee works
      - /r not implemented
      - /g has problems in certain backtracking zero-length-matches

  - regex variables $1, $2, ... and $&, $', $` work; other variables are not yet implemented.

  - capturing in zero-length-match has problems. Failing tests:

    ```
    t5/unit/regex_zero_length_match_capture.t
    t5/unit/regex_zero_length_match_match.t
    t5/unit/regex_zero_length_replace.t
    ```

  - Perlito5 allows underscores in named captures. This is not allowed in Java regex.

  - TODO - check this error message, this may need to be implemented for compatibility:

    `Unescaped left brace in regex is deprecated, passed through in regex; marked by <-- HERE in m/\G{ <-- HERE / at (eval 2) line 20.`


Eval-string
-----------

Limitations

  - eval compilation is slow; after compilation, the code runs at native speed.

  - eval bytecode is cached - this will leak memory
      - review the ClassLoader for leaks

  - some Java extensions are disabled inside eval-string - see section `Perlito5-Java extensibility`

Possible workarounds for slow compilation:

  - `ASM`
      - TODO: prototype eval-string with ASM

  - write a tiny interpreter for simple expressions

  - preload modules in `src5/util/jperl.pl`


Perlito5-Java extensibility
===========================

The Perlito5 Java backend doesn't support Perl XS extensions.
Instead of XS, it has an extension mechanism that connects Perl with Java.

TODO - investigate using the Nashorn convention for "Using Java from Scripts", see:
https://docs.oracle.com/javase/9/scripting/using-java-scripts.htm


`Java::inline` extension
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
  Object
  Object(0x34f22f9d)
  true
  ```

- example: retrieve a Java Class Object

  ```
  $ jrunscript -cp . -l Perl5 
  perl> eval { my $x = Java::inline q{ Class.forName("java.lang.Thread") }; say ref($x); say $x; }
  Class
  Class(0x3047254d)
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
    perl> my $x = Java::inline q{ new Integer(123) }
    123
    perl> my $x = Java::inline q{ (char)90 }
    Z
    perl> my $x; eval { $x = Java::inline q{ Class.forName("java.lang.Math") } }; say $x->PI
    3.141592653589793
    perl> my $x; eval { $x = Java::inline q{ Class.forName("java.lang.Integer") } }; say $x->MAX_VALUE
    2147483647
    perl> my $x; eval { $x = Java::inline q{ Class.forName("java.lang.Thread") } }; say $x->currentThread()
    Thread(0x262b2c86)
    perl> $x[10] = eval { Java::inline q{ Class.forName("java.lang.Thread") } }; say $x[10]->currentThread()
    Thread(0x5ed828d)
    ```

  - `new` invokes a constructor

  - Simple Java objects (String, Integer, Long, Double, Boolean) are converted to Perl values.

  - Other Java objects are seen by Perl as "blessed references"

  - Java exceptions can be catched with Perl eval-block

  - Note that explicitly returning `null` from `Java::inline` is a syntax error, because Java doesn't know how to dispatch the method call:

    ```
    perl> my $x; eval { $x = Java::inline q{ null } }; say $x
    /PlEval3.java:23: error: reference to set is ambiguous
      both method set(String) in PlLvalue and method set(System) in PlLvalue match
    ```


`Java` Perl module
-----------------

The `Java` Perl module is meant to emulate the `Java` global object in Nashorn.

```
$ jrunscript -cp .:perlito5.jar -l Perl5
perl> push @INC, "src5/lib";
1
perl> use Java

perl> Java->type("java.lang.Thread")
Class(0x704b2127)
perl> $Thread = Java->type("java.lang.Thread"); $Thread->new();
Thread(0x7ff2b8d2)

perl> sub UUID () { Java->type("java.util.UUID") }
perl> UUID->randomUUID()->toString()
9e8aadb3-4d81-41c6-af85-7ab7213a9945
```

  - this doesn't work yet:

    `Java->type("int[]")`


Java extensions in eval-string (work in progress)
-------------------------------------------------

  - Java objects can be assigned to Perl scalar variables, array elements, or hash elements.

  - These extensions are allowed in pre-compilation mode, but not in eval-string mode:

    - TODO - syntax for dereferencing scalars (Java objects are stored as references)

    - TODO - native Java variables (typed variables)

    - TODO - assign to array, assign to hash

    - TODO - syntax for "import" Java class

    - TODO - syntax for Java method calls - typed argument lists are work in progress

    - TODO - syntax for creating new Java subclass ("extends" and "implements")

  - Perl modules using extensions can be precompiled ahead-of-time in `perlito5.jar`, by adding a `use` statement in `src5/util/jperl.pl`

    - TODO - interoperation of "ahead-of-time" compiler extensions and "eval-string" extensions is untested


Java extensions in ahead-of-time (pre-compilation) mode
-------------------------------------------------------

Java classes can be added to a Perl script using a special "package" declaration:

```perl
package Sample { import => "misc.Java.Sample" };
```

  - an empty package works for importing builtin types or primitives ("String", "Long", "long")

  - an "import" specification works for importing Java classes

  - an "extends" specification works for adding methods to an existing class

  - an "implements" specification works for adding methods to an existing interface

  - a "header" specification works for creating a Java package


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
      
          Object o = engine.eval(" $x = 456; say 123 + $x; \"value was $x\" ");
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
  perl> $Perlito5::Java::DEBUG=1   # enable debugging output
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
    import => "java.util.ArrayList",
    java_type => "ArrayList<String>",
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
package Java::Array::Of::String {
    import => "java.util.ArrayList",
    java_type => "ArrayList<String>",
}

my Java::Object $obj = Java::Object->new();

my Java::Array::Of::String $arr = Java::Array::Of::String->new();
$arr->add("123");
$arr->add($p->toString());
```

- native arrays

Perl arrays can be assigned a native array:

```perl
package Byte { };
@perl_array = JavaCall->toBytes();
```

Native array variables can not be created directly.
As a workaround, see "Java::inline".

```perl
package String { };
my @arr2 = Java::inline ' new String[]{ "a", "b" } ';
print "arr2[0] $arr2[0], arr2[1] $arr2[1]\n";
```

- Constants

  - Character

    Perlito can't represent native `Character` values (only String)

    ```perl
    package Character { };
    my Character $b = "a";
    # error: incompatible types: String cannot be converted to Character
    ```

    workaround:

    ```perl
    package Character { };
    package String { };
    my Character $b = String->new("a")->charAt(0);
    my Character $b = $v->to_char();
    ```

  - Long

    Perlito can't represent native "Long" values (only Int):

    ```perl
    package Long {};
    my Long $b = 100;
    # error: incompatible types: int cannot be converted to Long
    ```

  - workaround:

    ```perl
    package Long {};
    my Long $b = Long->new(100.0);
    my Long $b = $v->to_long();
    ```

  - Float

    Perlito can't represent native "Float" values (only Double):

    ```perl
    package Float {};
    my Float $b = 100.0;
    # error: incompatible types: double cannot be converted to Float
    ```

    workaround:

    ```perl
    package Float {};
    my Float $b = Float->new(100.0);
    my Float $b = $v->to_float();
    ```


Using typed variables
---------------------

```perl
package long {}
my long $j;             # Java variable
my $var;                # Perl variable
$var = $j;              # store Java value in Perl variable
$j = $var->to_long();   # get Java value from Perl variable
```

Typed variables generate efficient, native Java. The catch is that there are a few restrictions:

- Java variables are not captured by Perl closures. This means that a variable declared in a context
will not be seen inside inner subroutine declarations (named or anonymous) and eval blocks. Loops and
conditionals should work fine, because these are not usually implemented as closures.

  - workaround: store the Java value in a Perl variable

- Java variables are not accepted as Perl subroutine parameters.

  - workaround: store the Java value in a Perl variable

- Java methods with type "void" should not be in the last line of a Perl block.
This is because Perl blocks return the last value, and "void" is not acceptable as a value.

  - workaround: add a plain-perl line, such as "return", "undef", or "1".

Extending a Java class with Perl
--------------------------------

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
    print "date_string: self is $self\n";       # prints date_string: self is My::Date(0x27ce2dd4)
    return "Hello";
}

my Java::Date $j_date = Java::Date->new();
my $s1 = $j_date->toString();   # original class
my My::Date $date = My::Date->new();
my $s2 = $date->toString();     # extended class

print $s1, " ", $s2, "\n";   # prints date and "Hello"
```


Thread safety
-------------

Perl global variables are shared between threads.
This includes for example: `$_`, `$a`, `$b`, `$/`, `@INC`, `%SIG`, `$0`, `$1`, `$&`, `$"`.
Perl variable `@_` (the parameter list) is a special case, it behaves internally like a lexical and it may be captured by closures.

"local" stack is shared.

Perlito also has an internal "boolean" stack, which is shared.

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

"make" rebuilds everything, including the java-eval-string and the nodejs-based compiler

- Perl-Java test suite

  ```bash
  $ make test-5java   # tests the Java-precompile backend
  $ make test-5jar    # tests the Java-eval-string backend (perlito5.jar)
  $ make test         # tests the nodejs backend
  ```

"make test" should pass everything, except for the "sleep" function which requires a nodejs module

"make test-5java" passes a few tests

- Syntax tree

  You may find useful when debugging,

  ```bash
  $ perl perlito5.pl -Isrc5/lib -Cast-json -e ' … '
  $ perl perlito5.pl -Isrc5/lib -Cast-perl5 -e ' … '
  ```

to see the internal representation

- Other

use "make clean" to get rid of all those .class files


Perlito5 Java backend TODO list
===============================


CPAN distribution
-----------------

This documentation should be copied to file Perlito5::Java, in the CPAN distribution.


Java-specific command line options
----------------------------------

  - specify main Class name (currently "Main")
  
  - have a way to port a simple .pm to a .java (without a main function)
  
      - specify input arguments

      - specify context (list, scalar, void)

      - specify what we want to return: PlObject vs. array of strings, etc
  

Workaround JVM bytecode size limit
----------------------------------

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

Perlito5-Java extensibility in "eval-string" mode
------------------------------------

- Introspection container

  When a new Java class is introduced inside "eval-string", it is too late to create a (static) type
  that is recognizable by the already compiled code.

  In this case we could use a container "PlJavaObject" and call methods using introspection.

- Syntax

  Jython, JRuby, Groovy and Nashorn provide some examples.

  - https://github.com/shekhargulati/java8-the-missing-tutorial/blob/master/10-nashorn.md

  Alternately, our own "pre-compile" mode introduced some syntax we could reuse.

  See also http://search.cpan.org/dist/Inline-Java/Java.pod

  - https://wiki.python.org/jython/UserGuide#interaction-with-java-packages

Perlito5-Java extensibility in "pre-compile" mode
------------------------------------

This documentation should be copied to file Perlito5::Java, in the CPAN distribution.

- Java import and typed variables

  ```perl
  package The::Class {
      import           => 'full.path.Class',  # mandatory
      java_type        => 'Class',            # auto generated, can be overridden: 'Class<String>'
      perl_to_java     => 'to_TheClass',      # auto generated from Perl package name, can be overridden
      # perl_package   => 'The::Class',       # auto generated, Perl package name
  }
  ```

- Java import

  ```perl
  package Sample {
      import => "misc.Java.Sample"
  };

  package Array::Of::String {
      import => "java.util.ArrayList",
      java_type => "ArrayList<String>",
  }
  ```

  - generates:
    - import misc.java.Sample;              (DONE)
    - adds a pObject coercion `to_Sample`   (DONE)
    - adds a pObject variant `pSample`      (DONE)
                                            (TODO: add argument list - test)
                                            (TODO: maybe unbox the arguments automatically)
    - add a pScalar variant `set(Sample)`   (TODO)
    - add pArray and pHash setters          (TODO)

  - TODO: what happens when a class is imported again
        - for example, import `Int` or `Byte` again

  - TODO: test that Perl modules can import Java classes
        - only tested in `main` program


- Typed variables

    `my TYPE VARLIST`
        - See also: http://perldoc.perl.org/functions/my.html

    - no `global` typed variables (only `my` variables)

  - Note:
        - parameters to native calls are evaluated in scalar context
        - untyped variables are passed by reference - that is, v_x instead of v_x.get()
        - wantarray() context is not passed to native calls

    ```bash
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package Sample { import => "misc.Java.Sample" }; my $x = Sample->new(); $x->to_Sample() ' > Test.java ; javac Test.java
    ```

    my $p_put = Sample->new();
    my $p_put = new Sample();

  - creates a boxed Java variable           (DONE)

    $x->to_Sample()

    retrieves the native Sample object      (DONE)
    allow conversion of primitive types - to_Int(), to_String()
                                            (TODO: generate primitive types in emitter)
  - this only works if $x is a Perl variable that contains a value of type "Sample"

    "$x"      # Sample<0x1234567>

    $x is a Perl variable that contains a native "Sample"; it behaves like a Perl object

    my $x = $p_put;

  - puts the boxed object into a Perl scalar  (DONE)

    my Sample $put = Sample->new();

  - creates a native Java variable          (DONE)
                                            (TODO: allow Int, String types)

    my Int $val = Sample->VAL;

  - "method call without parenthesis"

    read a class or instance variable

    my $x = $put;

  - puts the unboxed object into a Perl scalar  (DONE)

    my $x = Sample->new()

  - stores the boxed pSample object in a Perl scalar (DONE)

    ```
    package Int { import => 'java.lang.Integer' };
    my Int $x = 3;          # $x is a Java Int (not a pScalar)  (TODO: test)
    ```

  - maybe TODO: automatic casting `my Result $java_obj = $scan_result;`

  - maybe TODO: make pJavaReference which will have this implementation
        - Note: Boxed Java objects can be undef (null)

  - TODO: capture typed variables in closures

    maybe TODO: allow typed variables in parameter list
        but they would probably lose the type information

        possible workaround:

        my Int $x = $y;     # automatically insert a call to $y->toInt()

  - maybe TODO: call Perl subroutines with native parameters

        print $x->to_Sample();

  - TODO: (wip) call Java methods with Perl parameters

    ```perl
    Sample->new(10);                # native int
    Sample->new("abc");             # native String
    Sample->new($v->to_Sample());   # cast back to Sample
    Sample->new(0 + $v);            # cast to int
    Sample->new(0.0 + $v);          # cast to double
    Sample->new("" . $v);           # cast to string
    ```

  - Method chaining:

    ```
        my $global_queue = ConcurrentLinkedQueue::Of::String->new();
        my ConcurrentLinkedQueue::Of::String $queue = $global_queue->to_ConcurrentLinkedQueueOfString();
        my $x = $queue->poll();
    ```

  - but this doesn't work yet:

    ```
        my $x = $global_queue->to_ConcurrentLinkedQueueOfString()->poll();
        (TODO)
    ```

  - Automatic casting:

    ```
        # cast perl object to java object
        my Result $java_obj_result = $scan_result->to_Result();
    ```

  - would be:

    ```
        my Result $java_obj_result = $scan_result;
        (TODO)
    ```

  - Array-deref:

    ```
        @$native will retrieve an iterator and produce a Perl list
        (TODO)

        and "keys %$native" can be written like:
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
    
    PlLvalue.set() is super-overloaded # Array, Hash, int, string, boolean, ...

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
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package my::Sample { import => "misc.Java.Sample" }; my $x = my::Sample->new(); $x->to_mySample(); say "ref: ", ref($x), " x ", $x; my @arr = (1,2,5); say ref(\@arr); $x = \@arr; say ref($x); my my::Sample $z = my::Sample->new(); $x = $z; 
    ```

  - everything at the right side of ...->to_JavaThing()->... is native-call


- native expressions TODO

    ```perl
    $x++         # autoincrement

    $x = $x + 1  # assignment
                 # TODO - cast arguments to "number", "string" or "boolean" depending on operator

    print $x     # print
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

  - Conversion from Perl scalar to native array is not implemented.

  - Native array variables can not be created directly.

    ```perl
    # Possible implementation for creating native array variables
    #
    # my byte @bytes;
    # $#bytes = 9;      # indexes 0..9 (10 elements)
    ```

    ```perl
    # Not implemented: import a native array Class
    #
    #  package Java::Array::Of::String {
    #       import => "java.lang.String",
    #       java_type => "String[]",
    #   }
    ```

    ```perl
    # Not implemented: assign native array to Perl scalar
    #   (assign to Perl array works, see above)
    #
    # my $arr3 = Java::inline ' new String[]{ "a", "b" } ';
    # print "arr3[1] $arr3->[0]\n";
    # print "arr3[1] $arr3->[1]\n";
    ```

  - Conversion from Perl scalar to native array is not implemented.

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
      import => "java.util.Iterator",
      java_type => "Iterator<String>",
  };
  package ArrayList::Of::String {
     import => "java.util.ArrayList",
     java_type => "ArrayList<String>",
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
  my ArrayList::Of::String $arr = $bar->to_ArrayListOfString();
  my Iterator::Of::String $iterator = $arr->iterator();
  while($iterator->hasNext()) {
    my $element = $iterator->next();
    say $element;
  }
  ```

- coercing method naming rules

    - rule: remove '::', add 'to_'
        example:  my::Sample  =>  to_mySample()

      ```perl
      package my::Sample { import => "misc.Java.Sample" };
      my $x = my::Sample->new();
      $x->to_mySample()
      ```

- autobox as-needed

  - runtime methods should accept String, int, double, boolean types
    and maybe other types of number (byte, ...)

  - Array and Hash should accept other types of containers

    - String accepts char in constructor (DONE)
    - Hash accepts String for index (DONE)
    - Array accepts int for index   (DONE)


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


Overflow from int to double
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

  - bless (DONE)

  - `UNIVERSAL::`

        can
        isa
        DOES

  - (DONE) Scalar::blessed

  - TODO - unit tests (work in progress)

  - TODO - invalidate method cache when subroutine changes or @INC changes

- Perl features

  - overload

  - tie()

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

      - TODO - the compiler stack is "leaking" into the script stack. The "Perlito5" namespace belongs to the compiler.

        ```bash
        $ java -jar perlito5.jar -I src5/lib -e ' sub x { print "@{[ caller($_) ]}\n" for 0..3; } sub yy { eval "x()"; }; ( sub { yy() } )->(); '
        main -e 2 main::x
        main -e 2 main::yy
        Perlito5 src5/util/jperl.pl 9 Perlito5::eval_string

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
        0: BBB -e 2 AAA::x
        1: CCC -e 2 BBB::yy
        2: Perlito5 -e 2 CCC::cc
        3:  src5/util/jperl.pl 9 Perlito5::eval_string
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

  - TODO - per-thread: "local" stack, "boolean" stack, regex_result

Optimizations
-------------

  - use `our`-ish variables to avoid global variable lookups
        Note: remember the special-cases for `local` keyword

  - do-block and eval-block in void-context don't need a subroutine wrapper

  - don't pre-expand ranges in loops (DONE)

  - replace regex with index

  - use 'continue' and 'break' when possible (in place of Perl 'next', 'last')

  - identify variables that don't need a true `lvalue` container;
    store in a `PerlObject` instead of `PerlLvalue`,
    this is one less level of indirection.

  - investigate performance of "proxy" lvalues;
    when taking an lvalue out of an array or hash, return a proxy
    with a reference to the container and index.

  - investigate using Lambdas:
    https://stackoverflow.com/questions/26257266/are-java-8-lambdas-compiled-as-inner-classes-methods-or-something-else

Modules
-------

  - ported modules are in src5/lib/Perlito5X/Java

  - there is a port of JSON.pm - it is pure-perl and slow. It would be nice to have a native-java version.

  - TODO: DBI.pm



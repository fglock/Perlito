
Perlito5 Java backend
=====================


Requirements
------------

- Java 7

Perlito5 runtime uses "java.nio.file.Files", which was introduced in Java 7.

Java 7 is also required for named groups in regex, like:

~~~
    (?<name>X).
~~~

Perlito5-Java platform differences
-------------------------------------------

  - no timely destruction (DESTROY) (because we use Java memory management)
      - files don't "auto-close" at the end of a block
      - weaken() is a no-op
      - Try::Tiny "finally" doesn't work
      - Object::InsideOut will not cleanup unused objects

  - no XS (because we use Java instead of C)
      - many CPAN modules which use C libraries don't work
      - some CPAN modules are already ported, see: src5/lib/Perlito5X/Java/

  - some system features are not readily available in Java, such as:
      - file permissions for setuid, setgid, and sticky bit are not implemented
      - some signals are not available in Java.

  - eval-string not yet implemented, some operations will die()


Build using make
----------------

Update the Perl-based compiler "perlito5.pl"

~~~sh
    $ make build-5to5
~~~

TODO: add more options to Makefile

  - build-perlito5-lib-jar

  - build command-line "jperl"

  - build stand-alone, precompiled script

  - build android script


Compiling the compiler into a jar file
--------------------------------------

See also: make_perlito5-lib-jar.sh

TODO: add to Makefile

~~~sh
    $ make clean

    $ perl perlito5.pl --bootstrapping -Isrc5/lib -Cjava src5/util/perlito5.pl > perlito5.java

    $ time javac -J-Xms2000m -J-Xmx2000m -J-Xss2000m -source 7 perlito5.java
    warning: [options] bootstrap class path not set in conjunction with -source 1.7

    $ java Main -v
    This is Perlito5 9.021, an implementation of the Perl language.


    # create the perlito5.jar file
    $ rm perlito5.jar
    $ mkdir org
    $ mkdir org/perlito
    $ mkdir org/perlito/Perlito5
    $ mv *.class org/perlito/Perlito5/
    $ jar -cfe perlito5.jar org.perlito.Perlito5.Main org/perlito/Perlito5/*.class
    $ rm -rf org


    $ java -jar perlito5.jar -v
    This is Perlito5 9.021, an implementation of the Perl language.

    # run a test
    $ java -jar perlito5.jar --bootstrapping -Isrc5/lib -Cjava t5/unit/array.t > x.java ; javac -source 7 x.java ; java Main
    ok 1 ...

    # test the bootstrapping
    $ java -jar perlito5.jar --bootstrapping -Isrc5/lib -Cjava src5/util/perlito5.pl > x.java
    $ diff x.java perlito5.java
    [ no differences ]
~~~

  - Using the jar file

~~~java
    import org.perlito.Perlito5.*;
~~~

~~~sh
    $ javac -cp perlito5.jar JavaCompiler5.java
~~~

Perlito5-Java work-in-progress
------------------------------

  - "Java Scripting API"

      - https://docs.oracle.com/javase/8/docs/technotes/guides/scripting/prog_guide/api.html

  - Problems compiling with Java 8

      - it seems to trigger this problem:
          http://stackoverflow.com/questions/30707387/troubleshoot-slow-compilation
          http://stackoverflow.com/questions/34223249/slow-compilation-with-jooq-3-6-plain-sql-and-the-javac-compiler

          - "The workaround is to compile at Java 7-compatibility level: javac -source 7, or just to use simpler constructions.
          - "the workaround is to introduce local variables when there are nested generic method calls that use generic type inference

      - other compiler options that don't seem to work:

~~~sh
    $ time javac perlito5.java
    # never finishes

    $ time javac -source 7 perlito5.java
    warning: [options] bootstrap class path not set in conjunction with -source 1.7
    The system is out of resources.
    java.lang.StackOverflowError

    $ time javac -J-Xms1024m -J-Xmx1024m -J-Xss1024m -source 7 perlito5.java
    The system is out of resources.
    Consult the following stack trace for details.
    java.lang.OutOfMemoryError: Java heap space
~~~


  - BEGIN blocks
      - Loops containing: BEGIN blocks, "use" statements, or named subroutines.
          - lexical variables inside loops don't behave properly if they are captured at compile-time
      - lexical variables are not shared between closures created in BEGIN blocks

  - no eval-string at runtime (because not-yet-bootstrapped)
      - also no: "do FILE", "require" (because these depend on eval-string)

  - runtime error messages do not include the line number in the Perl code
      - also caller() is only partially implemented
      - BEGIN line numbers show the line number at the time of eval - the line number is relative to the start of the BEGIN block

  - no "goto LABEL"
      - "goto &code" works, but it doesn't do a tail-call

  - signals are partially implemented
      - $SIG{__WARN__} and $SIG{__DIE__} are implemented
      - other signals are not yet implemented.

  - object system is partially implemented
      - method resolution order is not selectable
      - method caching is not implemented
      - interaction between inheritance and overloading need more tests

  - tied variables are partially implemented
      - DESTROY not used, because we use Java memory management
      - tie scalar works
      - tie array incomplete
      - tie hash incomplete
      - tie filehandle todo

  - overload is partially implemented
      - overload string, number, boolean work
      - binary operators not implemented
      - mutators and assignment not implemented
      - dereferencing, iterators, filetest not implemented
      - "nomethod" not implemented
      - "fallback" not implemented; fallback mode behaves as "TRUE"

  - file handles are partially implemented
      - open scalarref works
      - <DATA> works
      - open binary mode vs. open utf8 needs more work
      - files don't "auto-close"

  - subroutines
      - "my sub x {...}" not implemented

  - lvalue $#a and other expressions: substr, ternary, chop, keys, pos

  - CORE::GLOBAL namespace

  - "local @_" doesn't work yet, because @_ is special

  - "~~" operator not implemented; also "when" and "given" not implemented.

  - incomplete implementations for sprintf(), pack(), unpack()

  - Perl threads not implemented
      - Java threads can be used, with some limitations. See: misc/Java/TestConcurrent.pl and misc/Java/TestThread2.pl


Regex differences
-----------------

  - regex modifiers /ismxgec work the same as Perl; other modifiers are not yet implemented.
      - /r not implemented
      - /ee not implemented, it depends on runtime eval-string

  - regex variables $1, $2, ... and $&, $', $` work; other variables are not yet implemented.

  - named captures cannot contain an underline in the name.
      Valid names must be composed of characters 'a'-'z', 'A'-'Z', '0'-'9'.

  - capturing in zero-length-match has problems. Failing tests:

~~~
        t5/unit/regex_zero_length_match_capture.t
        t5/unit/regex_zero_length_match_match.t
        t5/unit/regex_zero_length_replace.t
~~~

  - TODO - check this error message, this may need to be implemented for compatibility:
      Unescaped left brace in regex is deprecated, passed through in regex; marked by <-- HERE in m/\G{ <-- HERE / at (eval 2) line 20.


Eval-string (work in progress)
------------------------------

Limitations

  - eval bytecode is cached - this will leak memory

  - Java extensions are disabled (only plain-perl)

Instructions

  - create the "perlito5-lib.jar" file

~~~sh
    $ . make_perlito5-lib-jar.sh
~~~

  - compile the Perl script to Java with the "--java_eval" option

  - compile the Java script with perlito5-lib.jar in the classpath

  - run the class with perlito5-lib.jar in the classpath

~~~sh
    $ time perl perlito5.pl -Isrc5/lib/ --java_eval -Cjava -e ' say eval "123"; ' > x.java ; javac -cp perlito5-lib.jar -source 7 x.java ; java -cp '.:perlito5-lib.jar' Main
~~~

See also: misc/Java_eval$ vim JavaCompiler6.java

Compiling the compiler with BEGIN capabilities

  - preparation

~~~sh
    # update perlito5.pl, just in case
    $ make build-5to5

    # create "perlito5-lib.jar"
    $ . make_perlito5-lib-jar.sh
~~~

  - create a CLI

~~~sh
    $ perl perlito5.pl --bootstrapping --java_eval -Isrc5/lib -Cjava src5/util/jperl.pl > jperl.java

    $ time javac -cp .:perlito5-lib.jar -source 7 jperl.java

    # test
    $ java -cp '.:perlito5-lib.jar' Main -e ' say 123 '
~~~


Perlito5-Java extensibility
===========================


The Perlito5 Java backend doesn't support Perl XS extensions.
Instead of XS, it has an extension mechanism that connects Perl with Java.

Java classes can be added to a Perl script using a special "package" declaration:

  - an empty package works for importing builtin types or primitives ("String", "Long", "long")

  - an "import" specification works for importing Java classes

  - an "extends" specification works for adding methods to an existing class

  - an "implements" specification works for adding methods to an existing interface

  - a "header" specification works for creating a Java package

"Java::inline" can be used to add Java expressions to a Perl script


Calling a Perl subroutine from Java
-----------------------------------

~~~java
    class MyJava {
        public static void main(String[] args) throws Exception {
            Main.init();
            PlObject[] res = Main.apply("main::test", "123");
            for (PlObject s: res) {
                System.out.println("Java result: " + s.toString());
            }
        }
    }
~~~

Importing a Java class into Perl
--------------------------------

~~~perl
    package Sample {
        import => "misc.Java.Sample"
    };

    package Array::Of::String {
        import => "java.util.ArrayList",
        java_type => "ArrayList<String>",
    }

    my Array::Of::String $x = Array::Of::String->new();
~~~

Importing Java value types (Typed variables)
--------------------------------------------

Java value types don't need to be imported, but there must exist a Perl package:

~~~perl
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
~~~

All primitive data type declarations are supported:

~~~perl
    package long      { }
    package int       { }
    package short     { }
    package byte      { }
    package float     { }
    package double    { }
    package boolean   { }
    package char      { }
~~~

Other value types can be imported:

~~~perl
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
~~~

- native arrays

Perl arrays can be assigned a native array:

    package Byte { };
    @perl_array = JavaCall->toBytes();

Native array variables can not be created directly.
As a workaround, see "Java::inline".

~~~perl
    package String { };
    my @arr2 = Java::inline ' new String[]{ "a", "b" } ';
    print "arr2[0] $arr2[0], arr2[1] $arr2[1]\n";
~~~

- Constants

  - Character

    Perlito can't represent native "Character" values (only String)

~~~perl
    package Character { };
    my Character $b = "a";
    # error: incompatible types: String cannot be converted to Character
~~~

    workaround:

~~~perl
    package Character { };
    package String { };
    my Character $b = String->new("a")->charAt(0);
    my Character $b = $v->to_char();
~~~

  - Long

    Perlito can't represent native "Long" values (only Int):

~~~perl
    package Long {};
    my Long $b = 100;
    # error: incompatible types: int cannot be converted to Long
~~~

  - workaround:

~~~perl
    package Long {};
    my Long $b = Long->new(100.0);
    my Long $b = $v->to_long();
~~~

  - Float

    Perlito can't represent native "Float" values (only Double):

~~~perl
    package Float {};
    my Float $b = 100.0;
    # error: incompatible types: double cannot be converted to Float
~~~

    workaround:

~~~perl
    package Float {};
    my Float $b = Float->new(100.0);
    my Float $b = $v->to_float();
~~~


Using typed variables
---------------------

~~~perl
    package long {}
    my long $j;             # Java variable
    my $var;                # Perl variable
    $var = $j;              # store Java value in Perl variable
    $j = $var->to_long();   # get Java value from Perl variable
~~~

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

~~~perl
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
~~~


Using Java inside Perl code with Java::inline
---------------------------------------------

Example: Java method override using Java::inline

~~~perl
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
~~~

Thread safety
-------------

Perl global variables are shared between threads.
This includes for example: $_, $a, $b, $/, @INC, %SIG, $0, $1, $&, $".
Perl variable @_ (the parameter list) is not shared.

"local" stack is shared.

Perlito also has an internal "boolean" stack, which is shared.

Perl lexical variables are not shared between threads.
Variables captured in closures running in different threads
are shared.

Perl operators are not atomic.


Perlito5 Java development tools
===============================

* Execute a single-line quick test 

~~~bash
$ perl perlito5.pl -Isrc5/lib -Cjava -e ' sub x { return 123, 5 } my $x = x(); say "$x" ' > Main.java ; javac Main.java ; java Main
5
~~~

* Rebuild the compiler

~~~bash
$ make build-5to5 
~~~

"make" rebuilds everything, including the nodejs-based compiler

* Perl-Java test suite

~~~bash
$ make test-5java
$ make test  # tests the nodejs backend
~~~

"make test" should pass everything, except for the "sleep" function which requires a nodejs module

"make test-5java" is just starting to pass a few tests

* Syntax tree

You may find useful when debugging,

~~~bash
$ perl perlito5.pl -Isrc5/lib -Cast-json -e ' … '
$ perl perlito5.pl -Isrc5/lib -Cast-perl5 -e ' … '
~~~

to see the internal representation

* Other

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
  
    specify input arguments

    specify context (list, scalar, void)

    specify what we want to return: PlObject vs. array of strings, etc
  

Add 'eval string' support
-------------------------

  - JS-eval-string: embedding a JavaScript-in-Java interpreter:
        https://github.com/fglock/Perlito/blob/master/misc/Java/TestJS.pl

  - Java-eval-string: using the native compiler API:
        https://github.com/fglock/Perlito/blob/master/misc/Java_eval/JavaCompiler4.java

  - ASM:
        TODO: prototype eval-string with ASM

Workaround JVM bytecode size limit
----------------------------------

According to the Java Virtual Machine specification,
the bytecode of a method must not be bigger than 65536 bytes:

  - See: $Perlito5::CODE_TOO_LARGE in the /src5

  - Test.java:2309: error: code too large

  - when compiling misc/Java/code_too_large.pl

  - possible workaround: insert a closure every 100s of lines in a block:

~~~perl
        code...
        return (sub {
                code ...
                return (sub {
                        code ...
                    }->() )
            }->() )
~~~

Document Perlito5-Java extensibility
------------------------------------

This documentation should be copied to file Perlito5::Java, in the CPAN distribution.

    - Java import and typed variables

~~~perl
        package The::Class {
            import           => 'full.path.Class',  # mandatory
            java_type        => 'Class',            # auto generated, can be overridden: 'Class<String>'
            perl_to_java     => 'to_TheClass',      # auto generated from Perl package name, can be overridden
            # perl_package   => 'The::Class',       # auto generated, Perl package name
        }
~~~

- Java import

~~~perl
    package Sample {
        import => "misc.Java.Sample"
    };

    package Array::Of::String {
        import => "java.util.ArrayList",
        java_type => "ArrayList<String>",
    }
~~~

    generates:
    - import misc.java.Sample;              (DONE)
    - adds a pObject coercion "to_Sample"   (DONE)
    - adds a pObject variant "pSample"      (DONE)
                                            (TODO: add argument list - test)
                                            (TODO: maybe unbox the arguments automatically)
    - add a pScalar variant "set(Sample)"   (TODO)
    - add pArray and pHash setters          (TODO)

    TODO: what happens when a class is imported again
        - for example, import "Int" or "Byte" again

    TODO: test that Perl modules can import Java classes
        - only tested in "main" program


- Typed variables

    my TYPE VARLIST
        - See also: http://perldoc.perl.org/functions/my.html

    - no "global" typed variables (only "my" variables)

    Note:
        - parameters to native calls are evaluated in scalar context
        - untyped variables are passed by reference - that is, v_x instead of v_x.get()
        - wantarray() context is not passed to native calls

~~~bash
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package Sample { import => "misc.Java.Sample" }; my $x = Sample->new(); $x->to_Sample() ' > Test.java ; javac Test.java
~~~

    my $p_put = Sample->new();
    my $p_put = new Sample();
    creates a boxed Java variable           (DONE)

    $x->to_Sample()
    retrieves the native Sample object      (DONE)
    allow conversion of primitive types - to_Int(), to_String()
                                            (TODO: generate primitive types in emitter)
    this only works if $x is a Perl variable that contains a value of type "Sample"

    "$x"      # Sample<0x1234567>
    $x is a Perl variable that contains a native "Sample"; it behaves like a Perl object

    my $x = $p_put;
    puts the boxed object into a Perl scalar  (DONE)

    my Sample $put = Sample->new();
    creates a native Java variable          (DONE)
                                            (TODO: allow Int, String types)

    my Int $val = Sample->VAL;
    "method call without parenthesis"
    read a class or instance variable

    my $x = $put;
    puts the unboxed object into a Perl scalar  (DONE)

    my $x = Sample->new()
    stores the boxed pSample object in a Perl scalar (DONE)

    package Int { import => 'java.lang.Integer' };
    my Int $x = 3;          # $x is a Java Int (not a pScalar)  (TODO: test)

    maybe TODO: automatic casting (my Result $java_obj = $scan_result;)

    maybe TODO: make pJavaReference which will have this implementation
        - Note: Boxed Java objects can be undef (null)

    TODO: capture typed variables in closures

    maybe TODO: allow typed variables in parameter list
        but they would probably lose the type information

        possible workaround:

        my Int $x = $y;     # automatically insert a call to $y->toInt()

    maybe TODO: call Perl subroutines with native parameters
        print $x->to_Sample();

    TODO: (wip) call Java methods with Perl parameters

~~~perl
        Sample->new(10);                # native int
        Sample->new("abc");             # native String
        Sample->new($v->to_Sample());   # cast back to Sample
        Sample->new(0 + $v);            # cast to int
        Sample->new(0.0 + $v);          # cast to double
        Sample->new("" . $v);           # cast to string
~~~

    Method chaining:

        my $global_queue = ConcurrentLinkedQueue::Of::String->new();
        my ConcurrentLinkedQueue::Of::String $queue = $global_queue->to_ConcurrentLinkedQueueOfString();
        my $x = $queue->poll();

    but this doesn't work yet:

        my $x = $global_queue->to_ConcurrentLinkedQueueOfString()->poll();
        (TODO)

    Automatic casting:

        # cast perl object to java object
        my Result $java_obj_result = $scan_result->to_Result();

    would be:

        my Result $java_obj_result = $scan_result;
        (TODO)

    Array-deref:

        @$native will retrieve an iterator and produce a Perl list
        (TODO)

        and "keys %$native" can be written like:
        @{ $native->entrySet() }
        @{ $native->keySet() }
        @{ $native->values() }
        (TODO)

    Array-deref in boolean context:
        
        Automatic call to ->hasNext on iterator reference in
        boolean context. The idea is that instead
        
            while ($iterator->hasNext()) { ... }

         we can type:

            while (@$bar) { ... }

         First, the @$bar dereferencing of native java list would obtain
         an iterator reference and then due to having iterator reference in
         boolean context, the call to hasNext() would autmatically happen.
         That way we will be able to write idiomatic Perl loops on
         native java Lists

    Native values in Perl expressions:

        in this case, we can just assign the value to PlLvalue
        because PlLvalue knows what to do with each type
        this also takes care of "$x" and 0+$x
        (TODO)
        
        PlLvalue.set() is super-overloaded # Array, Hash, int, string, boolean, ...
        src5/lib/Perlito5/Java/Runtime.pm 1463

        (TODO) comparison and logical operators with Java variables:
        if ($typed == $typed) ...
        if ($typed eq $typed) ...
        if ($typed || $typed) ...
        if ($typed && $typed) ...

- interoperation between native expressions and perl expressions

    method calls on typed variables call Java methods,
    method calls on untyped variables call Perl methods

    subroutines are always "Perl"

    Class method calls on imported classes are "Java",
    all other Class method calls are "Perl".

    Parameters to Java method calls should be converted to Java automatically.  (TODO: test, examples)

    storing a Java object into a Perl scalar converts the value to a "Perl" object (blessed reference);
    Java "int", "String", "double" are converted to Perl values.

    using Java objects in other types of expressions than scalar assignment is not well defined. (TODO: test, examples)

    using Perl objects in Java code is not well defined. (TODO: test, examples)

    storing a Java object into a typed variable keeps the Java object as-is.

    test case:

~~~bash
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package my::Sample { import => "misc.Java.Sample" }; my $x = my::Sample->new(); $x->to_mySample(); say "ref: ", ref($x), " x ", $x; my @arr = (1,2,5); say ref(\@arr); $x = \@arr; say ref($x); my my::Sample $z = my::Sample->new(); $x = $z; 
~~~

    everything at the right side of ...->to_JavaThing()->... is native-call


- native expressions TODO

~~~perl
    $x++         # autoincrement

    $x = $x + 1  # assignment
                 # TODO - cast arguments to "number", "string" or "boolean" depending on operator

    print $x     # print
~~~

    test case:

~~~perl
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
~~~


Value types
---------------

Conversion from Perl scalar to native array is not implemented.

Native array variables can not be created directly.

~~~perl
    # Possible implementation for creating native array variables
    #
    # my byte @bytes;
    # $#bytes = 9;      # indexes 0..9 (10 elements)
~~~

~~~perl
    # Not implemented: import a native array Class
    #
    #  package Java::Array::Of::String {
    #       import => "java.lang.String",
    #       java_type => "String[]",
    #   }
~~~

~~~perl
    # Not implemented: assign native array to Perl scalar
    #   (assign to Perl array works, see above)
    #
    # my $arr3 = Java::inline ' new String[]{ "a", "b" } ';
    # print "arr3[1] $arr3->[0]\n";
    # print "arr3[1] $arr3->[1]\n";
~~~

Conversion from Perl scalar to native array is not implemented.

~~~perl
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
~~~

    See: toArray(T[] a) in https://docs.oracle.com/javase/7/docs/api/java/util/ArrayList.html

in Perl:

    char::Array, boolean::Array, float::Array, double::Array, long::Array, short::Array, int::Array, byte::Array

~~~perl
    # alternate implementation:
    #
    # package String { };
    # my String::Array = String::Array->new(@array)
~~~
 
~~~bash
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package byte::Array { type => 'byte[]' } my byte::Array $x = byte::Array->new("A","B","C");'
~~~

        - this should return:

~~~java
        byte[] v_x = new byte[3]; // alocate 3 bytes - the native arrays in java are not dynamic
        byte[0] ='A';
        byte[1] = 'B';
        byte[2] = 'C';
~~~

        - or:

~~~java
        byte[] v_x = new byte[] { "A", "B", "C" };
~~~

    alternately:

~~~perl
        package my_chars { type => 'char[]' }
~~~

    Investigate adding support for plain "Object" arguments.
        See http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html arguments


- mixing Java containers, Perl, and Java::inline
    See: misc/Java/TypedIterator.pl

~~~perl
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
~~~

- coercing method naming rules

    - rule: remove '::', add 'to_'
        example:  my::Sample  =>  to_mySample()

~~~perl
    package my::Sample { import => "misc.Java.Sample" };
    my $x = my::Sample->new();
    $x->to_mySample()
~~~

- autobox as-needed

    runtime methods should accept String, int, double, boolean types
    and maybe other types of number (byte, ...)

    Array and Hash should accept other types of containers

    - String accepts char in constructor (DONE)
    - Hash accepts String for index (DONE)
    - Array accepts int for index   (DONE)


Autovivification of aliased parameters
--------------------------------------

(DONE)

~~~bash
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
~~~

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

    -- TEST
    %hash = (foo => 11, bar => 22, baz => 33); $scalar = delete @hash{qw(foo bar)}; print "$scalar\n"
    %hash = (foo => 11, bar => 22, baz => 33); @array = delete @hash{qw(foo bar)}; print "@array\n"

    %hash = (foo => 11, bar => 22, baz => 33); $scalar = @hash{qw(foo bar)}; use feature 'say'; say $scalar
    %hash = (foo => 11, bar => 22, baz => 33); @array = @hash{qw(foo bar)}; use feature 'say'; say "@array"

    %hash = (foo => 11, bar => 22, baz => 33); print "@hash{qw(foo bar)}"

    my $scalar = 11; @hash{qw(foo bar)} = $scalar; use feature "say"; say $hash{foo}
    my @array = (11, 22); @hash{qw(foo bar)} = @array; use feature "say"; say "@array"

Variables
---------

    delete local EXPR

    subroutine lookups could also be "our"-like (also method lookups)


Overflow from int to double
---------------------------

    partially implemented - needs more work

    Note: integer operations may have problems with type erasure
    example:
        $i + subr();  # subr() returns pObject instead of pInt
    this needs more tests


Tail-call
---------

    caller sends a parameter to enable tail-call
    callee pushes the call to a stack
    caller pops from the stack and call the closures in order

- tailcalls

    same-subroutine tailcalls could execute a "redo" in the current subroutine.

Missing features, or partially implemented, or untested
-------------------------------------------------------

- Object-related

    bless (DONE)
    UNIVERSAL::
        can
        isa
        DOES
    (DONE) Scalar::blessed

    TODO - unit tests (work in progress)

    TODO - method dispatch (current impl doesn't look up classes in @INC)

    TODO - method cache

    TODO - invalidate method cache when subroutine changes or @INC changes

- Perl features

    overload
    tie()
    exceptions
        "goto", "last", "redo"
        Note: remember to cleanup the stacks (boolean, local).
    "continue" blocks
    "dualvars"
        for string-to-number optimization
    unicode-strings vs. byte-strings
    subroutine signatures
    return (list)
    assignment to splice
    (DONE) sprintf
        http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...)
        http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
    pack
    unpack

    typeglob operations
        TODO - add tests

    caller

        $ perl -e ' sub x { print "in " . __PACKAGE__ . "\n"; v() }  x(); { package X; *x = \&main::x; x() } sub v { $v = caller(); print "called from $v\n" } '
        in main
        called from main
        in main
        called from main


    __DATA__ sections

        %Perlito5::DATA_SECTION contains the __DATA__ for each package

    add test:
    ' print " ${\__PACKAGE__} \n" '

- Add tests

    NaN, Inf, -0

    @perl_array = java_native[]
        supported types: byte[], int[], and imported_class[]
        not implemented: long[], String[], Double[], char[]

Regex
-----

Regex variables: named captures (TODO - add tests)

Quotemeta: \Q

Modifiers are not serialized yet (DONE - add tests)

~~~bash
$ perl -e ' my $user_agent_regexp = "123";  my $regexp = qr/$user_agent_regexp/x; print $regexp; '
(?^x:123)
~~~

but a qr// on an existing qr// behaves differently:

~~~bash
$ perl -le ' my $x = qr/ (\w) /; my $z = qr/$x/x; print $z '
(?^: (\w) )

$ perl -le ' my $x = qr/ (\w) /; my $z = qr/before $x after/x; print $z '
(?^x:before (?^: (\w) ) after)
~~~

See also:

    "Replace Rhino regexp implementation with java.util.regex"
    https://bugzilla.mozilla.org/show_bug.cgi?id=390659

    DFA
    http://www.brics.dk/automaton/index.html

    Mini-language with fast regex, Artistic License
    http://jint.sourceforge.net

    regex reference:
    http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html

    http://www.regular-expressions.info/reference.html

    Discussion about alternative implementations:
    http://stackoverflow.com/questions/415580/regex-named-groups-in-java

Threads
-------

    See: http://perldoc.perl.org/perlmod.html#Making-your-module-threadsafe
        CLONE, CLONE_SKIP

    global variables should be cloned in threads,
    use this to get the thread id: Thread.currentThread().getId()

    Examples:
    misc/Java/TestConcurrent.pl
    misc/Java/TestThread.pl
    misc/Java/TestThread2.pl    # atomic


- pass the threadid as a parameter to all closures;

    closure can use that thread id to get/set the value from globals hash.

    TODO - per-thread: "local" stack, "boolean" stack, regex_result

Optimizations
-------------

  - use "our"-ish variables to avoid global variable lookups
        Note: remember the special-cases for "local" keyword

  - memoize method-name lookups

  - do-block and eval-block in void-context don't need a subroutine wrapper

  - don't pre-expand ranges in loops (DONE)

  - replace regex with index

  - use 'continue' and 'break' when possible (in place of Perl 'next', 'last')

  - identify variables that don't need a true "lvalue" container;
    store in a "PerlObject" instead of "PerlLvalue",
    this is one less level of indirection.

  - investigate performance of "proxy" lvalues;
    when taking an lvalue out of an array or hash, return a proxy
    with a reference to the container and index.


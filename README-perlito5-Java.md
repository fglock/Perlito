
Perlito5 Java backend
=====================


Perlito5-Java platform differences
-------------------------------------------


  - no timely destruction (DESTROY) (because Java)
      - files don't "auto-close" at the end of a block
      - Try::Tiny "finally" doesn't work

  - no XS (because Java)

  - limited BEGIN blocks side-effects (because unfinished Perlito5 impl)
      - "import" also doesn't work when doing precompilation
      - subroutines need to be declared before use

  - no eval-string (because not-yet-bootstrapped)
      - also no: "do FILE", "require" (because these depend on eval-string)

  - any other differences are not-yet-implemented or bugs.


Regex differences
-----------------

Perlito5 compiles Perl regexes into Java regexes with some wrapping code.
Some differences between the regex engines will show up:

  - named captures

    Java 7 is required for named groups in regex, like: (?<name>X).
    Discussion about alternative implementations:
    http://stackoverflow.com/questions/415580/regex-named-groups-in-java

    Named captures in Java cannot have an underline in the name.
    Valid names must be composed of characters 'a'-'z', 'A'-'Z', '0'-'9'.

  - regex comments with "(?#text)" are not implemented in Java

See also:

    http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html#jcc

    http://www.regular-expressions.info/reference.html


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
    package String { }
    package Integer { }
    package Boolean { }
    package Double { }
    package Short { }
    package Byte { }
    
    my String $x  = "abc";
    my Integer $y = 123;
    # assigning a Java value to a Perl variable
    my $v = Boolean->TRUE;
    print "$v\n";   # 1
~~~

These primitive data type declarations are supported:

~~~perl
    package long      { }
    package int       { }
    package short     { }
    package byte      { }
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
    package J::Date   { import => "java.util.Date" };
    
    # create and import the extended class
    package My::Date {
        extends => 'J::Date',
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
    
    my J::Date $j_date = J::Date->new();
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
This includes for example: $_, $a, $b, $/, @INC.
Perl variable @_ (the parameter list) is not shared.

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

Primitive data types
------------

implemented:

~~~perl
    package long      { }
    package int       { }
    package short     { }
    package byte      { }
~~~

other primitive types are missing.


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
  
  - TODO: fix: module compilation requires the "bootstrapping" switch

    remove the need for --bootstrapping

    perl perlito5.pl -I src5/lib --bootstrapping -Cjava -e ' use Data::Dumper; print Data::Dumper::Dumper [ 123 , { 4 => 4 } ]'

Add 'eval string' support
-------------------------

  - JS-eval-string: embedding a Javascript-in-Java interpreter:
        https://github.com/fglock/Perlito/blob/master/misc/Java/TestJS.pl

  - Java-eval-string: using the native compiler API:
        https://github.com/fglock/Perlito/blob/master/misc/Java_eval/JavaCompiler4.java

  - ASM:
        TODO: prototype eval-string with ASM

Workaround JVM bytecode size limit
----------------------------------

According to the Java Virtual Machine specification,
the bytecode of a method must not be bigger than 65536 bytes:

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

-- Java import

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


-- Typed variables

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

-- interoperation between native expressions and perl expressions

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

    maybe TODO: everything at the right side of ...->to_JavaThing()->... is native-call


-- native expressions TODO

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

Object

    Not implemented; there is a commented-out patch in Perlito5::Java::Emitter

Character

    Perlito can't represent native "Character" values (only String)

Long

    Perlito can't represent native "Long" values (only Int):

    my Long $b = 100;

    Main.java:3341: incompatible types
    found   : int
    required: java.lang.Long

Float

    Perlito can't represent native "Float" values (only Double):

    my Float $b = 100.0;

    Main.java:3282: incompatible types
    found   : double
    required: java.lang.Float

primitive Java types

    char, boolean, float, long, short, int, byte

    in Perl:
    char::Array, boolean::Array, float::Array, long::Array, short::Array, int::Array, byte::Array

    @perl_array = JavaCall->toBytes();  (DONE - autobox Java array into a Perl array)

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


-- mixing Java containers, Perl, and Java::inline
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

-- coercing method naming rules

    - rule: remove '::', add 'to_'
        example:  my::Sample  =>  to_mySample()

~~~perl
    package my::Sample { import => "misc.Java.Sample" };
    my $x = my::Sample->new();
    $x->to_mySample()
~~~

-- autobox as-needed

    runtime methods should accept String, int, double, boolean types
    and maybe other types of number (byte, ...)

    Array and Hash should accept other types of containers

    - String accepts char in constructor (DONE)
    - Hash accepts String for index (DONE)
    - Array accepts int for index   (DONE)


Autovivification of aliased parameters
--------------------------------------

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

    'state'

    delete local EXPR

    subroutine lookups could also be "our"-like (also method lookups)

    (DONE) subroutine parameter lists should be list-of-aliases

      - create a special PlArray constructor that stores lvalues

      - See: PlArray.construct_list_of_aliases()

Symbolic references
-------------------

~~~bash
    $ perl -e ' $a = 123; my $z; $z = "a"; print $$z '
    123
    $ perl -e ' my $a = 123; my $z; $z = "a"; print $$z '
    ''
~~~

Overflow from int to double
---------------------------

    partially implemented - needs more work

    Note: integer operations may have problems with type erasure
    example:
        $i + subr();  # subr() returns pObject instead of pInt
    this needs more tests

    internal code should use "long" instead of "int" (DONE)

Tail-call
---------

    caller sends a parameter to enable tail-call
    callee pushes the call to a stack
    caller pops from the stack and call the closures in order

Missing features, or partially implemented, or untested
-------------------------------------------------------

Object-related

    bless (DONE)
    UNIVERSAL::
        can
        isa
        DOES
    AUTOLOAD
    Scalar::blessed

    TODO - unit tests
    TODO - method dispatch (current impl doesn't look up classes in @INC)
    TODO - method cache
    TODO - invalidate method cache when subroutine changes or @INC changes

Perl features

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
    (DONE) rw @_ in subroutines
    rw $_ in loops
    sprintf
        http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...)
        http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
    pack
    unpack
    file operations
    pos()
    typeglob operations

-- Add tests

    NaN, Inf, -0

    @perl_array = java_native[]
        supported types: byte[], int[], and imported_class[]
        not implemented: long[], String[], Double[], char[]

Regex
-----

regex reference: http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html

Regex variables: $1, $2 and named captures

Modifiers:

    /x (DONE)

    /e (TODO)

Quotemeta: \Q

Modifiers are not serialized yet

~~~java
public String toString() {
   // TODO - show flags
   return this.original_string;
}
~~~

that is:

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

Threads
-------

    See: http://perldoc.perl.org/perlmod.html#Making-your-module-threadsafe
        CLONE, CLONE_SKIP

    global variables should be cloned in threads,
    use this to get the thread id: Thread.currentThread().getId()

    Examples:
    misc/Java/TestConcurrent.pl
    misc/Java/TestThread.pl

Optimizations
-------------

  - use "our"-ish variables to avoid global variable lookups
        Note: remember the special-cases for "local" keyword

  - memoize method-name lookups

  - do-block and eval-block in void-context don't need a subroutine wrapper

  - don't pre-expand ranges in loops

  - replace regex with index

  - use 'continue' and 'break' when possible (in place of Perl 'next', 'last')

  - identify variables that don't need a true "lvalue" container;
    store in a "PerlObject" instead of "PerlLvalue",
    this is one less level of indirection.

  - investigate performance of "proxy" lvalues;
    when taking an lvalue out of an array or hash, return a proxy
    with a reference to the container and index.
    Note: there is a working, partial implementation in PerlOp.push_local()



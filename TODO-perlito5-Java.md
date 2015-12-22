Perlito5 Java backend TODO list
===============================

Java-specific command line options
----------------------------------

    specify main Class name (currently "Main")

-- TODO: fix: module compilation requires the "bootstrapping" switch

    remove the need for --bootstrapping

    perl perlito5.pl -I src5/lib --bootstrapping -Cjava -e ' use Data::Dumper; print Data::Dumper::Dumper [ 123 , { 4 => 4 } ]'

Add 'eval string' support
-------------------------

    JS-eval-string: embedding a Javascript-in-Java interpreter:
        https://github.com/fglock/Perlito/blob/master/misc/Java/TestJS.pl

    Java-eval-string: using the native compiler API:
        https://github.com/fglock/Perlito/blob/master/misc/Java_eval/JavaCompiler4.java

    ASM:
        TODO: prototype eval-string with ASM

Workaround JVM bytecode size limit
----------------------------------

According to the Java Virtual Machine specification,
the bytecode of a method must not be bigger than 65536 bytes:

    Test.java:2309: error: code too large

    when compiling misc/Java/code_too_large.pl

    possible workaround: insert a closure every 100s of lines in a block:

        code...
        return (sub {
                code ...
                return (sub {
                        code ...
                    }->() )
            }->() )

Document Perlito5-Java platform differences
-------------------------------------------

This documentation should be copied to file Perlito5::Java, in the CPAN distribution.

    - no timely destruction (DESTROY) (because Java)
        - files don't "auto-close" at the end of a block
        - Try::Tiny "finally" doesn't work

    - no XS (because Java)
    - limited BEGIN blocks side-effects (because unfinished Perlito5 impl)
        - "import" also doesn't work when doing precompilation
    - no eval-string (because not-yet-bootstrapped)
        - also no: "do FILE", "require" (because these depend on eval-string)

    - any other differences are not-yet-implemented or bugs.

Document Perlito5-Java extensibility
------------------------------------

This documentation should be copied to file Perlito5::Java, in the CPAN distribution.

The Perlito5 Java backend doesn't support Perl XS extensions.
It has an extension mechanism that connects Perl with Java.

    - Java import and typed variables

        package The::Class {
            import           => 'full.path.Class',  # mandatory
            java_type        => 'Class',            # auto generated, can be overriden: 'Class<String>'
            perl_to_java     => 'to_TheClass',      # auto generated from Perl package name, can be overriden
            # perl_package   => 'The::Class',       # auto generated, Perl package name
        }


-- Java import

    package Sample {
        import => "misc.Java.Sample"
    };

    package Array::Of::String {
        import => "java.util.ArrayList",
        java_type => "ArrayList<String>",
    }

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


-- Java method override using Java::inline

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


-- Typed variables

    my TYPE VARLIST
        - See also: http://perldoc.perl.org/functions/my.html

    - no "global" typed variables (only "my" variables)

    Note:
        - parameters to native calls are evaluated in scalar context
        - untyped variables are passed by reference - that is, v_x instead of v_x.get()
        - wantarray() context is not passed to native calls

    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package Sample { import => "misc.Java.Sample" }; my $x = Sample->new(); $x->to_Sample() ' > Test.java ; javac Test.java

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
        Sample->new(10);                # native int
        Sample->new("abc");             # native String
        Sample->new($v->to_Sample());   # cast back to Sample
        Sample->new(0 + $v);            # cast to int
        Sample->new(0.0 + $v);          # cast to double
        Sample->new("" . $v);           # cast to string

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

    Native values in Perl expressions:

        in this case, we can just assign the value to PlLvalue
        because PlLvalue knows what to do with each type
        this also takes care of "$x" and 0+$x
        (TODO)
        
        PlLvalue.set() is super-overloaded # Array, Hash, int, string, boolean, ...
        src5/lib/Perlito5/Java/Runtime.pm 1463

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
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package my::Sample { import => "misc.Java.Sample" }; my $x = my::Sample->new(); $x->to_mySample(); say "ref: ", ref($x), " x ", $x; my @arr = (1,2,5); say ref(\@arr); $x = \@arr; say ref($x); my my::Sample $z = my::Sample->new(); $x = $z; 

    maybe TODO: everything at the right side of ...->to_JavaThing()->... is native-call


-- primitive types

    char, boolean, float, long, short, int, byte

    in Perl:
    char::Array, boolean::Array, float::Array, long::Array, short::Array, int::Array, byte::Array

    @perl_array = JavaCall->toBytes();  (DONE - autobox Java array into a Perl array)

    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' package byte::Array { type => 'byte[]' } my byte::Array $x = byte::Array->new("A","B","C");'

        - this should return:
        byte[] v_x = new byte[3]; // alocate 3 bytes - the native arrays in java are not dynamic
        byte[0] ='A';
        byte[1] = 'B';
        byte[2] = 'C';

        - or:
        byte[] v_x = new byte[] { "A", "B", "C" };

    alternately:

        package my_chars { type => 'char[]' }

    Investigate adding support for plain "Object" arguments.
        See http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html arguments


-- mixing Java containers, Perl, and Java::inline
    See: misc/Java/TypedIterator.pl

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


-- coercing method naming rules

    - rule: remove '::', add 'to_'
        example:  my::Sample  =>  to_mySample()

    package my::Sample { import => "misc.Java.Sample" };
    my $x = my::Sample->new();
    $x->to_mySample()

-- autobox as-needed

    runtime methods should accept String, int, double, boolean types
    and maybe other types of number (byte, ...)

    Array and Hash should accept other types of containers

    - String accepts char in constructor (DONE)
    - Hash accepts String for index (DONE)
    - Array accepts int for index   (DONE)


Autovivification of aliased parameters
--------------------------------------

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

Symbolic references
-------------------

    $ perl -e ' $a = 123; my $z; $z = "a"; print $$z '
    123
    $ perl -e ' my $a = 123; my $z; $z = "a"; print $$z '
    ''

Overflow from int to double
---------------------------

    partially implemented - needs more work

    Note: integer operations may have problems with type erasure
    example:
        $i + subr();  # subr() returns pObject instead of pInt
    this needs more tests

    internal code should use "long" instead of "int"

Tail-call
---------

    caller sends a parameter to enable tail-call
    callee pushes the call to a stack
    caller pops from the stack and call the closures in order

Missing features, or partially implemented, or untested
-------------------------------------------------------

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
    rw @_ in subroutines
    rw $_ in loops
    sprintf
        http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...)
        http://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
    pack
    unpack
    file operations
    regex
        http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
    pos()
    typeglob operations

-- Add tests

    NaN, Inf, -0

    @perl_array = java_native[]
        supported types: byte[], int[], and imported_class[]
        not implemented: long[], String[], Double[], char[]

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

    use "our"-ish variables to avoid global variable lookups
        Note: remember the special-cases for "local" keyword

    memoize method-name lookups

    do-block and eval-block in void-context don't need a subroutine wrapper

    don't pre-expand ranges in loops

    replace regex with index

    use 'continue' and 'break' when possible (in place of Perl 'next', 'last')

    identify variables that don't need a true "lvalue" container;
    store in a "PerlObject" instead of "PerlLvalue",
    this is one less level of indirection.

    investigate performance of "proxy" lvalues;
    when taking an lvalue out of an array or hash, return a proxy
    with a reference to the container and index.
    Note: there is a working, partial implementation in PerlOp.push_local()



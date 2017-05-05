"Perlito5" Perl 5 compiler
==========================

This is Perlito5, a compiler that implements a subset of Perl 5.

Perlito5 translates Perl to Java, and Perl to JavaScript.

Build using make
----------------

    make
        - builds perlito5.js (which runs in node.js)

    make test
        - tests perlito5.js using node.js

    make build-5browser
        - builds html/perlito5.js (which runs in the browser)

- See [Makefile](Makefile) for more options


Compiling and running using Java
--------------------------------

- plase see:

      - [README-perlito5-Java](README-perlito5-Java.md)

Running the tests using "node.js"
---------------------------------

    # this command will compile "perlito5.js"
    perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js

    # this will run a single test script
    node perlito5.js -Isrc5/lib t5/01-perlito/01-sanity.t

    # this will run all tests
    prove -r -e 'node perlito5.js -I./src5/lib' t5


Compile the compiler to JavaScript into perlito5.js
---------------------------------------------------

- using perl and perlito5.pl:

    perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js

- using node.js and perlito5.js:

    node perlito5.js -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5-new.js

Compile the compiler to Perl5 using perl
----------------------------------------

    perl perlito5.pl -I./src5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl

Compile perlito5-in-browser using perl
--------------------------------------

    perl util-js/make-perlito5-js.sh


Running the tests using "perl"
------------------------------

    # this will run all tests
    prove -r -e 'perl perlito5.pl -I./src5/lib ' t5


Bootstrap with perl
-------------------

    time perl perlito5.pl -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && diff perlito5-new.pl perlito5.pl ; cp perlito5-new.pl perlito5.pl

Bootstrap with node.js
----------------------

    time node perlito5.js -Isrc5/lib -Cjs src5/util/perlito5.pl > perlito5-new.js && diff perlito5-new.js perlito5.js ; cp perlito5-new.js perlito5.js


Minifying the javascript output
-------------------------------

    The "jsmin" compressor gives 20% compression:
    
      http://crockford.com/javascript/jsmin
    
    $ sudo port install jsmin   # osx
    $ jsmin < perlito5.js > mini-perlito5.js
    $ nice prove -r -e 'node mini-perlito5.js -I./src5/lib' t5
    ...
    All tests successful.



Perlito5 TODO list
==================

CPAN distribution
-----------------

- create Markdown files for github documentation;
- example: http://www.unexpected-vortices.com/sw/rippledoc/quick-markdown-example.html
- in CPAN, convert all the documentation to POD using one of these:

    $ perl -e ' use Markdown::To::POD "markdown_to_pod"; my @text = <>; my $pod = markdown_to_pod(join "", @text); print $pod; ' README

    $ perl -e ' use Markdown::Pod;my $m2p = Markdown::Pod->new; my @text = <>; my $pod = $m2p->markdown_to_pod(markdown => join "", @text); print $pod; ' README


Command-line options
--------------------

- implement -i switch and ARGVOUT

- shebang processing:

~~~perl
    #!/usr/bin/perl -pi.orig
    s/foo/bar/;
~~~

Libraries
---------

- these should go into namespace Perlito5X::*

- Test (implemented as Perlito5::Test)

- Data::Dumper (implemented as Perlito5X::Dumper)

- create perlito5-specific libs for:

    feature.pm
    Config.pm
    overload.pm
    bytes.pm
    integer.pm
    lib.pm
    Carp.pm
    Tie::Array
    Tie::Hash
    Tie::Scalar
    Symbol

    alternately, check $^H for strictness - such that perl's own strict.pm just works
    and ${^WARNING_BITS} for warnings


Parser
------

- BEGIN blocks

  - Loops containing: BEGIN/INIT/END blocks, "use" statements, or named subroutines

    lexical variables inside loops don't behave properly if they are captured at compile-time

    See BEGIN_SCRATCHPAD in src5/

~~~sh
    t5/unit/begin_global_special_var.t .......... Failed 1/2 subtests 
    t5/unit/begin_loop.t ........................ Failed 2/3 subtests 
    t5/unit/begin_recurse.t ..................... Failed 5/6 subtests 
~~~

  - dump-to-AST work in progress - src5/lib/Perlito5/DumpToAST.pm

    - tied variables not yet supported

    - overloaded values may cause problems

    - shared captures (shared lexicals) are not shared

~~~sh
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cperl5  -e ' use strict; BEGIN { my $y = 123; my $z = 456;for my $x (1..3) { no strict "refs"; *{"x$x"} = sub { print "here\n"; eval q{ print "y $y\n" }; $y; return $x } } }  x1(); '
~~~

    - blessed array/scalar/code is not supported (also in Data::Dumper)

    - prototypes are not set

    - subroutine predeclaration needs tests

    - circular references are not tested

    - test if dump-to-AST interferes with Java extensions

  - eval-string code in src5/lib/Perlito5/Perl5/Emitter.pm
    should be moved to src5/lib/Perlito5/AST/CompileTime.pm

        if ($code eq 'eval' && $Perlito5::PHASE eq 'BEGIN') {

- parse example in http://www.perlmonks.org/?node_id=663393

~~~sh
    $ perl perlito5.pl -I src5/lib -Cperl5 -e ' whatever  / 25 ; # / ; die "this dies!"; '
        whatever(m! 25 ; # !);
        die('this dies!')
    $ perl -MO=Deparse -e ' whatever  / 25 ; # / ; die "this dies!"; '
        'whatever' / 25;
    $ perl -e ' print whatever  / 25 ; # / ; die "this dies!"; '
        this dies! at -e line 1.
~~~

- "'" meaning "::"

~~~perl
    $'m  # $::m
    $m'  # String found where operator expected

    package X'Y  # X::Y
    package X'   # Invalid version format (non-numeric data)
~~~

- attributes
    http://perldoc.perl.org/attributes.html
    missing MODIFY_CODE_ATTRIBUTES handlers

- prototypes (signatures)
    http://perldoc.perl.org/perlsub.html#Prototypes

    code that depends on prototypes being (re)defined later - this breaks when the program is pre-compiled,
    because prototypes become stubs

~~~perl
    # t/test.pm

    sub like   ($$@) { like_yn (0,@_) }; # 0 for -      # this breaks if like_yn() is predeclared
    sub unlike ($$@) { like_yn (1,@_) }; # 1 for un-
    
    sub like_yn ($$$@) {
~~~

    check that undeclared barewords give the right error
    *foo = sub () { ... }   # does prototype work here?
    check signature in sort()
    fix the prototype for 'stat(*)' (see t/test.pl in the perl test suite)

    '&@' - See Try::Tiny

- "namespace" parsing
    tests: t5/01-perlito/26-syntax-namespace.t

~~~sh
    $ perl -e ' { package X; sub print { CORE::print(">$_[1]<\n") } } my $x = bless {}, "X"; print $x "xxx" '
    Not a GLOB reference at -e line 1.

    $ perl -e ' { package X; sub printx { CORE::print(">$_[1]<\n") } } my $x = bless {}, "X"; printx $x "xxx" '
    >xxx<

    $ perl -MO=Deparse -e ' print X:: "xxx" '
    print X 'xxx';

    $ perl -e ' use strict; my $x = X::; print $x '
    X

    $ perl -e ' use strict; my $x = X; print $x '
    Bareword "X" not allowed while "strict subs" in use

    $ perl perlito5.pl -MO=Deparse -e ' ::X::x::y '
    join("", ::{'main::X::'} x main::y);

    $ perl -MO=Deparse -e ' ::X '
    '???';

    $ perl -MO=Deparse -e ' sub X {} ::X '
    sub X { }
    X;

    $ perl -e ' $::X::::X = 3; print $main::X::::X '        # 3
    $ perl -e ' $::X::::X = 3; print $main::main::X::::X '  # 3
    $ perl -e ' $::X::::X = 3; print $main::X::main::X '    # empty
    $ perl -e ' $::X::::X = 3; print $main::X::X '          # empty
    $ perl -e ' $::X::::X = 3; print $::::X::::X '          # empty
~~~

- CORE:: namespace can be used with operators:

~~~perl
    $ perl -MO=Deparse -e ' $x CORE::and $v '
    $v if $x;

    $ perl -MO=Deparse -e ' @v = CORE::qw/ a b c / '
    @v = ('a', 'b', 'c');

    $ perl -MO=Deparse -e ' $x CORE::+ $v '
    CORE:: is not a keyword
~~~

- strict and warnings: create options like 'subs', 'refs'

- things that work in perlito5, but which are errors in 'perl'

~~~sh
    string interpolation with nested quotes of the same type:

        $ perl -e ' " $x{"x"} " '
        String found where operator expected at -e line 1, near "x"} ""

        In perl5.22.0:
        Missing right curly or square bracket at -e line 1, within string
~~~

- __DATA__ and __END__ can be anywhere in the line

~~~sh
    $ perl -e 'print 123 __END__ x xx + '
    123

    $ perl -e 'print 123 + __END__ x xx + '
    syntax error at -e line 1, at EOF
~~~


Add tests for fixed bugs
------------------------

    unit tests:

    local() vs. next/redo/last

    our()

    aliasing inside for-loop

    aliasing of subroutine parameters

    prototypes

- postfix-dereferencing with @*

    See: http://www.effectiveperlprogramming.com/2014/09/use-postfix-dereferencing/

- things that work in perlito5, but which are errors in 'perl'

~~~sh
    $ perl -e ' $c (f) '
    syntax error at -e line 1, near "$c ("
~~~

    ---
    sigils in blocks

    Allowed:

~~~sh
    $ perl -e ' ${ for (1,2,3) {} } '
    $ perl -e ' @{ for (1,2,3) {} } '
    $ perl -e ' %{ for (1,2,3) {} } '
    $ perl -e ' *{ for (1,2,3) {} } '
    $ perl -e ' $#{ for (1,2,3) {} } '
    
    $ perl -e ' $; = 3; print ${ ; } '
    3
    $ perl -e ' $} = 3; print ${ } } '
    3
~~~

    Not allowed:

~~~sh
    $ perl -e ' \{ for (1,2,3) {} } '
    syntax error at -e line 1, near "{ for "
    Execution of -e aborted due to compilation errors.
    
    $ perl -e ' $a{ for (1,2,3) {} } '
    syntax error at -e line 1, near "{ for "

    $ perl -e ' print ${ } '
    syntax error at -e line 1, near "${ "
    Execution of -e aborted due to compilation errors.

    ---
    syntax error in hashref subscript - parses 's' as s///:
    ${x->{s}}
    $x->{s}
~~~

    ---
    wrong precedence in keys()

    if (keys %hh != 2) { print "not" }
    parses as: keys(%hh != 2)

    ---
    quotes vs. hash lookups:

        $ perl -e '  q}} '
        # ok

        $ perl -e ' $x{ q}} } '
        Unmatched right curly bracket at -e line 1, at end of line

        $ perl -e ' $x{ q]] } '
        # ok

    ---
    syntax error in ${@} in string
    "${@}";
    ${"};

    syntax error in autoquote: $x{s}
    parses as: $x { s/// }

    ---
    prototypes can change during compilation

    $ perl -e ' my @x; sub z0 ($$@) { zz(99, @_) } sub zz ($$$@) { print "@_\n" } @x=(9,4,6); zz(8,5,@x); z0(8,5,@x); '
    8 5 3           # zz prototype is '$$$@'
    99 8 5 9 4 6    # zz prototype was '@' when z0 was compiled


    ---
    resolve "filehandle" methods

    $ perl -e ' package X; open(X, ">", "x"); X->print(1234) '
    $ cat x
    1234
    $ nodejs perlito5.js -Isrc5/lib -Cjs -e ' package X; open(X, ">", "x"); X->print(1234) '


    ---
    variable declarations in expressions

    our $Verbose ||= 0;
    our (%Cache);
    my $args = @_ or @_ = @$exports;


    ---
    add additional variants of "for"

    # "our" with localization
    $ perl -e ' use strict; our $x = 123; for our $x (1,2,3) { 2 } print "$x\n" '
    123

    # variable in scope with localization
    $ perl -e ' use strict; my $x = 123; for $x (1,2,3) { 2 } print "$x\n" '123
    123


    ---
    add "print", "printf", "say" special parsing - note this is related to indirect object notation

    indirect object notation
    http://lwn.net/Articles/451486/
    http://www.modernperlbooks.com/mt/2009/08/the-problems-with-indirect-object-notation.html
    http://shadow.cat/blog/matt-s-trout/indirect-but-still-fatal/
    http://perlbuzz.com/mechanix/2008/02/the-perils-of-perl-5s-indirect.html

    method Module $param;
    new Class( arg => $value );
    new Class::($args);
    say $config->{output} "This is a diagnostic message!";  # indirect call
    say {$config->{output}} "This is a diagnostic message!"; # say to filehandle

    use Class;
    sub Class {
    warn 'Called Class sub not Class package';
    'Class'
    }
    my $q = Class->new; # calls the Class sub above
    my $s = new Class; # throws a 'Bareword found where operator expected' error
    my $t = Class::->new # this works
    my $u = new Class::; # this also works (even with sub main in the current package)

    sbertrang++ noted this is also valid:
    print( STDERR "123" )


    ---
    add tests for signatures: "empty" _ $ ;$

    ---
    add test for "sub _" should be in package "main"
    $ perl -MO=Deparse -e ' package X; sub _ { 123 } '
    package X;
    sub main::_ {
        123;
    }

    ---
    add test for defined-or vs. m//  (2012/9/25 Конрад Боровски <notifications@github.com>)
    Note: fixed; see test t5/01-perlito/25-syntax-defined-or.t

    $ perl perlito5.pl -Isrc5/lib -Cast-perl5 -e ' shift // 2 '
    Number or Bareword found where operator expected

    $ perl perlito5.pl -Isrc5/lib -Cast-perl5 -e ' shift / 2 '
    Can't find string terminator '/' anywhere before EOF

    ---
    add test for filetest operators special case:
    ' -f($file).".bak" ' should be equivalent to -f "$file.bak"
    parses as -(f($file)).".bak"
    but: ' -f ($file).".bak" '
    parses correctly
    This seems to be because there is a rule that \w followed by '(' is a function call;
    this needs more testing: ' ... and(2) '
    Test: redefine 'and', 'not' and check what works.

    '  $s111++ + $s222 '
    parses as  (+$s222)++

    '  $step++ < $steps '
    Can't find string terminator '>' anywhere before EOF

    ---
    -- from moritz, Schwern and others at
    http://stackoverflow.com/questions/161872/hidden-features-of-perl

    - you can use letters as delimiters

    $ perl -Mstrict  -wle 'print q bJet another perl hacker.b'
    Jet another perl hacker.

    Likewise you can write regular expressions:

    m xabcx
    # same as m/abc/

    ---
    $ perl -e ' my @things= map {  no warnings; 123 } @list; '
    "no" not allowed in expression
    (DONE) no longer a bug, this now works in perl5.22.0

- test that "use" checks the return value of modules (the "1;" thing)


Perl6 backend
-------------

- Running the tests using perl6:

~~~sh
    # TODO - this is not implemented yet
    . util-perl6/setup-perlito5-perl6.sh
    find t5/01-perlito/*.t | perl -ne ' print "*** $_"; chomp; print ` perl perlito5.pl -I./src5/lib -Cperl6 $_ > tmp.p6 && perl6 tmp.p6  ` '
~~~


- keep comments

- context: wantarray, return-comma

        sub x { return 7, 8 }
    vs. sub x { return (7, 8) }

    use an "out-of-band" parameter to set the call context, like:
    $v = x( :scalar )   # 8
    $v = x( :list   )   # 2

- <> is lines()

- 0..$#num to @num.keys

- choose @*ARGS or @_ in shift() and pop()

- typeglob assignment

- "given" statement not implemented

- refactoring sub arguments

    my $x = $_[0];
    my ($x, $y, @rest) = @_;    # check if @_ is unused elsewhere in the sub

- placeholder

    my ($a, $, $c) = 1..3;
    ($a, *, $c) = 1..3;

- __PACKAGE__

- specialized refactoring for packages that introduce syntax

    Try::Tiny
    List::MoreUtils
    Moose

- no strict

- bless

- tests



Perl5 backend
-------------

- "given" statement not implemented
- "default" statement not implemented

- ${^NAME} needs curly-escaping

- ${^GLOBAL_PHASE} is not writeable
    workaround in set_global_phase()

- fix regex delimiters, or escape the regexes

    test case:

    $ perl perlito5.pl -Isrc5/lib -I. -It -Cperl5 -e ' s/\$a$a/b$b/g; tr/\$c$c/d$d/; m/\$e$e/; ' 


Compile-time execution environment
----------------------------------

- work in progress

- test case:

~~~sh
    # 'our' variable is not seen:
    
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' use Data::Dumper; @x = (2..4, ";)"); print Dumper \@x '  > Main.java ; javac Main.java ; java Main
    $VAR1 = [
        2,
        3,
        4,
        chr(59) . chr(41),
    ];

    Subroutine "Perlito5::Dumper::escape_string" is not using the hash "%safe_char".
~~~    
    
~~~sh
    # captured lexical is not initialized:
    
    $ cat > X.pm
    my %safe = (a=>1); sub dump { $safe{a} }
    1;
    $ perl perlito5.pl -I. -Isrc5/lib -Cperl5 -e ' use X; '
    *main::dump = do {
        package main;
        my $safe = undef;   # TODO - initialize captured lexical
        sub {
            $safe{'a'}
        }
    };
~~~

- compile-time eval() is not bound to the "program" environment, but to the "compiler" environment instead
    see README-perlito5-js near "Compile-time / Run-time interleaving"

    my $v;
    BEGIN { $v = "123" }
    use Module $v;  # $v is not accessible at compile-time


    Test case:

    $ cat > X.pm
    package X;
    sub import { print "args [ @_ ]\n" }
    1;

    $ perl perlito5.pl -I src5/lib -I . -Cperl5 -e ' my $v; BEGIN { $v = "xxx" } use X $v; '
    # args [ X  ]

    expected result:
    # args [ X xxx ]


- work in progress: test BEGIN time serialization

    $ perl perlito5.pl -I src5/lib -Cperl5 -e ' my ($x, $y); { $x }; my $z; @aaa = @X::xxx + $bbb; BEGIN { $aaa = [ 1 .. 5 ]; $bbb = { 5, $aaa }; $ccc = sub { my %x; 123 } } $/; my $s; BEGIN { $s = 3 } BEGIN { *ccc2 = \$ccc; } '

    TODO - identify aliases: [[[ BEGIN { *ccc2 = \$ccc; } ]]] dumps:

          $main::ccc2 = $main::ccc;
        instead of:
          *main::ccc2 = \$main::ccc;

    - lexicals and closures are not dumped

    TODO - lexicals are not shared

         - maybe save lexical variable AST. This will help identify shared lexicals


- special backend option "_comp" dumps the compile-time execution environment:

~~~sh
    $ perl perlito5.pl -Isrc5/lib -I. -It -C_comp -e '  (0, undef, undef, @_)[1, 2] ; { 123 } sub x { 456; { 3 } }'
    {
        'block' => [
            {
                'block' => [],
            },
            {
                'block' => [
                    {
                        'block' => [],
                    },
                ],
                'name' => 'main::x',
                'type' => 'sub',
            },
        ],
    }

    $ perl perlito5.pl -Isrc5/lib -I. -It -C_comp -e ' local (undef, undef, @_) ; { 123 } sub x { 456; { my $x = 3 } } local $y; INIT { 123 } BEGIN { $Perlito5::SCOPE->{block}[-1]{xxx} = 3 }'
        change the environment using a BEGIN block
~~~

Nice to Have
------------

- keep comments in AST

- debugging symbols
- line numbers in error messages

- caller()
- "when"

- run more of the "perl" test suite

- proper "use strict" and "use warnings"
- use the same error messages and warnings as 'perl'

    $ perl -e ' my @x = $x %x '
    Operator or semicolon missing before %x at -e line 1.
    Ambiguous use of % resolved as operator % at -e line 1.
    Illegal modulus zero at -e line 1.

- no warnings 'redefine';

- __LINE__, __FILE__

- INIT{}, END{}
   look at the implementation in perlito6-in-Go

- source code - remove Perl 6 code such as "token"
   (fixed: This is only loaded if the grammar compiler is needed)

- *{ $name }{CODE}->();
    (DONE in java)

- local(*{$caller."::a"}) = \my $a;

- *{$pkg . "::foo"} = \&bar;

- local $SIG{__WARN__};

- bug https://github.com/fglock/Perlito/issues/10
    "Perlito 5 JS has syntax errors"

    Tried

    YUI Compressor online
    and
    Google Closure Compiler
    http://closure-compiler.appspot.com/home

    Both failed with syntax errors.

- parse the regexes
    Note: implemented in Perlito5::Grammar::Regex5
    create an AST for regexes


Oddities
--------

- from moritz, Schwern and others at
    http://stackoverflow.com/questions/161872/hidden-features-of-perl

    - you can give subs numeric names if you use symbolic references

    $ perl -lwe '*4 = sub { print "yes" }; 4->()'
    yes

    $ perl -e ' sub x { print 123 } x->() '
    Undefined subroutine &main::1 called at -e line 1.
    123

- return value of continue-block

    $ perl -e ' sub x {  { 456 } continue { 789 } } print "@{[ x() ]}\n" '
    456 789

Deprecate
---------

- Interpreter backend
   this is not being maintained; the code is still in src5/lib/Perlito5/Eval.pm just in case

   alternately, use the interpreter to compute constant foldings


"Perlito5" Perl to Java compiler and Perl to JavaScript compiler
==========================

This is Perlito5, a compiler that implements the Perl 5 programming language.

Perlito5 translates Perl to Java, and Perl to JavaScript.

Build using make
----------------

`make`

  - builds perlito5.js (which runs in node.js)
  - builds perlito5.jar (which runs in java)
  - builds html/perlito5.js (which runs in the browser)

`make test`

  - tests perlito5.js using node.js

`make build-5browser`

  - builds html/perlito5.js (which runs in the browser)

- See [Makefile](Makefile) for all `make` options


Compiling and running using Java
--------------------------------

- please see: [README-perlito5-Java](README-perlito5-Java.md)

Running the tests using "node.js"
---------------------------------

- this command will compile "perlito5.js" without using `make`

  `perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js`

- this will run a single test script

  `node perlito5.js -Isrc5/lib t5/01-perlito/01-sanity.t`

- this will run all tests without using `make`

  `prove -r -e 'node perlito5.js -I./src5/lib' t5`


Compile the compiler to JavaScript (`perlito5.js`)
---------------------------------------------------

- using perl and perlito5.pl:

  `perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js`

- using node.js and perlito5.js:

  `node perlito5.js -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5-new.js`

Compile the compiler to Perl5 using perl
----------------------------------------

  `perl perlito5.pl -I./src5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl`

Running the tests using "perl"
------------------------------

- this will run all tests without using `make`

  `prove -r -e 'perl perlito5.pl -I./src5/lib ' t5`


Bootstrap with perl
-------------------

  `make boot-5to5`

Bootstrap with node.js
----------------------

  `make boot-5js`


Minifying the javascript output
-------------------------------

The "jsmin" compressor gives 20% compression:
    
  http://crockford.com/javascript/jsmin
  
  ```
  $ sudo port install jsmin   # osx
  $ jsmin < perlito5.js > mini-perlito5.js
  $ nice prove -r -e 'node mini-perlito5.js -I./src5/lib' t5
  ...
  All tests successful.
  ```


Perlito5 TODO list
==================

Some things in this TODO list are already implemented, but missing tests of documentation.

CPAN distribution
-----------------

- in CPAN, convert all the github documentation to POD using one of these:

  ```
    $ perl -e ' use Markdown::To::POD "markdown_to_pod"; my @text = <>; my $pod = markdown_to_pod(join "", @text); print $pod; ' README

    $ perl -e ' use Markdown::Pod;my $m2p = Markdown::Pod->new; my @text = <>; my $pod = $m2p->markdown_to_pod(markdown => join "", @text); print $pod; ' README
  ```

Command-line options
--------------------

- add tests

- implement -i switch and ARGVOUT

- shebang processing:

  ```perl
    #!/usr/bin/perl -pi.orig
    s/foo/bar/;
  ```

Libraries
---------

- these go into namespace `Perlito5X::*`

- add tests for core modules


Parser
------

- Regexes

  - /x modifier is seen too late, after variable interpolation is processed:

    ```sh
    $ perl perlito5.pl -I src5/lib -Cast -e ' use strict; /abc # $v/x '
    Global symbol "$v" requires explicit package name
    ```

    ```sh
    $ java -jar perlito5.jar -Isrc5/lib  -e ' use strict; my $v; my $x; $x = qr/abc # $v/x; print $x; '
    (?x-ism:abc # )
    ```

    expected:

    ```sh
    perl -e ' use strict; my $v; my $x; $x = qr/abc # $v/x; print $x; '
    (?^x:abc # $v
    )
    ```

- BEGIN blocks

  - Loops containing: BEGIN/INIT/END blocks, "use" statements, or named subroutines

    lexical variables inside loops don't behave properly if they are captured at compile-time

    See BEGIN_SCRATCHPAD in src5/

    ```sh
    t5/unit/begin_global_special_var.t .......... Failed 1/2 subtests 
    t5/unit/begin_loop.t ........................ Failed 2/3 subtests 
    t5/unit/begin_recurse.t ..................... Failed 5/6 subtests 
    ```

  - dump-to-AST work in progress - src5/lib/Perlito5/DumpToAST.pm

    - tied variables not yet supported

    - overloaded values may cause problems

    - shared captures (shared lexicals) are not shared

      ```sh
      $ perl perlito5.pl -Isrc5/lib -I. -It -Cperl5  -e ' use strict; BEGIN { my $y = 123; my $z = 456;for my $x (1..3) { no strict "refs"; *{"x$x"} = sub { print "here\n"; eval q{ print "y $y\n" }; $y; return $x } } }  x1(); '
      ```

    - blessed regex/code/glob is not supported (also in Data::Dumper)

    - subroutine predeclaration needs tests

    - circular references are not tested

    - test if dump-to-AST interferes with Java extensions

  - eval-string code in src5/lib/Perlito5/Perl5/Emitter.pm
    should be moved to src5/lib/Perlito5/AST/CompileTime.pm

        if ($code eq 'eval' && $Perlito5::PHASE eq 'BEGIN') {

- parse example in http://www.perlmonks.org/?node_id=663393

  ```sh
  $ perl perlito5.pl -I src5/lib -Cperl5 -e ' whatever  / 25 ; # / ; die "this dies!"; '
      whatever(m! 25 ; # !);
      die('this dies!')
  $ perl -MO=Deparse -e ' whatever  / 25 ; # / ; die "this dies!"; '
      'whatever' / 25;
  $ perl -e ' print whatever  / 25 ; # / ; die "this dies!"; '
      this dies! at -e line 1.
  ```

- unicode characters in identifiers

  - See: https://stackoverflow.com/questions/4800275/what-characters-are-allowed-in-perl-identifiers/4800711#4800711

- `'` meaning `::`

  ```perl
  $'m  # $::m
  $m'  # String found where operator expected

  package X'Y  # X::Y
  package X'   # Invalid version format (non-numeric data)
  ```

- attributes

  http://perldoc.perl.org/attributes.html

  missing MODIFY_CODE_ATTRIBUTES handlers

- prototypes (signatures)

  http://perldoc.perl.org/perlsub.html#Prototypes

  code that depends on prototypes being (re)defined later - this breaks when the program is precompiled,
  because prototypes become stubs

  ```perl
  # t/test.pm

  sub like   ($$@) { like_yn (0,@_) }; # 0 for -      # this breaks if like_yn() is predeclared
  sub unlike ($$@) { like_yn (1,@_) }; # 1 for un-
  
  sub like_yn ($$$@) {
  ```

  check that undeclared barewords give the right error

  `*foo = sub () { ... }   # does prototype work here?`

  check signature in sort()
  fix the prototype for `stat(*)` (see t/test.pl in the perl test suite)

  `&@` - See Try::Tiny

- "namespace" parsing

  tests: t5/01-perlito/26-syntax-namespace.t

  ```sh
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
  ```

- CORE:: namespace can be used with operators:

  ```perl
  $ perl -MO=Deparse -e ' $x CORE::and $v '
  $v if $x;

  $ perl -MO=Deparse -e ' @v = CORE::qw/ a b c / '
  @v = ('a', 'b', 'c');

  $ perl -MO=Deparse -e ' $x CORE::+ $v '
  CORE:: is not a keyword
  ```

  possible implementation:

  ```
  diff --git a/src5/lib/Perlito5/Grammar/Precedence.pm b/src5/lib/Perlito5/Grammar/Precedence.pm
  index f29a8de3..ecde73ff 100644
  --- a/src5/lib/Perlito5/Grammar/Precedence.pm
  +++ b/src5/lib/Perlito5/Grammar/Precedence.pm
  @@ -54,7 +54,7 @@ sub add_term {
   my $End_token;
   my $End_token_chars;
   my %Op;
  -my @Op_chars = (4, 3, 2, 1);
  +my @Op_chars = (9, 8, 4, 3, 2, 1);     # CORE::and CORE::or
   sub op_parse {
       my $str  = shift;
       my $pos  = shift;
  @@ -243,9 +243,9 @@ add_op( 'list', [ ',' ],   $prec, { assoc => 'list' } );
   $prec = $prec - 1;
   add_op( 'prefix', [ 'not' ], $prec );
   $prec = $prec - 1;
  -add_op( 'infix', [ 'and' ], $prec );
  +add_op( 'infix', [ 'and', 'CORE::and' ], $prec );
   $prec = $prec - 1;
  -add_op( 'infix', [ 'or', 'xor' ], $prec );
  +add_op( 'infix', [ 'or', 'xor', 'CORE::or', 'CORE::xor' ], $prec );
   $prec = $prec - 1;
   add_op( 'infix', [ '*start*' ], $prec );
  ```

- strict and warnings: create options like 'subs', 'refs'

- things that work in perlito5, but which are errors in 'perl'

  string interpolation with nested quotes of the same type:

  ```sh
  $ perl -e ' " $x{"x"} " '
  String found where operator expected at -e line 1, near "x"} ""

  In perl5.22.0:
  Missing right curly or square bracket at -e line 1, within string
  ```

- error messages depend on eval context

  ```sh
  $ perl  -e ' use strict;  eval { YY GG }; print $@; sub YY {} '
  Can't locate object method "YY" via package "GG" (perhaps you forgot to load "GG"?) at -e line 1.
  
  $ perl  -e ' use strict;  eval " YY GG "; print $@; sub YY {} '
  Bareword "GG" not allowed while "strict subs" in use at (eval 1) line 1.
  
  $ perl  -e ' use strict;  eval " eval { YY GG } "; print $@; sub YY {} '
  Bareword "GG" not allowed while "strict subs" in use at (eval 1) line 1.
  ```

- each statement can have a single label for 'last', 'goto'.
  Multiple labels are not supported.

Add tests for fixed bugs
------------------------

- unit tests:

  local() vs. next/redo/last

  our()

  aliasing inside for-loop

  aliasing of subroutine parameters

  prototypes


- compiler hints with $^H

  ```sh
  $ perl -e ' $^H = 1; { $^H = 3; use strict; print "HERE $^H\n"; eval q{ print "EVAL $^H\n";  }; BEGIN {  print "BEGIN1 $^H\n";  } };  print $^H, "\n";  {  use strict; print "HERE2 $^H\n"; eval q{ print "EVAL2 $^H\n";  BEGIN {  eval q{ print "BEGIN-EVAL $^H\n" }  }    }; } '
  BEGIN1 2018
  HERE 3
  EVAL 3
  3
  HERE2 3
  BEGIN-EVAL 2018
  EVAL2 3
  ```

- postfix-dereferencing with `@*`

  http://www.effectiveperlprogramming.com/2014/09/use-postfix-dereferencing/

- things that work in perlito5, but which are errors in 'perl'

  ```sh
    $ perl -e ' $c (f) '
    syntax error at -e line 1, near "$c ("
  ```

  - sigils in blocks

    Allowed:

    ```sh
    $ perl -e ' ${ for (1,2,3) {} } '
    $ perl -e ' @{ for (1,2,3) {} } '
    $ perl -e ' %{ for (1,2,3) {} } '
    $ perl -e ' *{ for (1,2,3) {} } '
    $ perl -e ' $#{ for (1,2,3) {} } '
    
    $ perl -e ' $; = 3; print ${ ; } '
    3
    $ perl -e ' $} = 3; print ${ } } '
    3
    ```

    Not allowed:

    ```sh
    $ perl -e ' \{ for (1,2,3) {} } '
    syntax error at -e line 1, near "{ for "
    Execution of -e aborted due to compilation errors.
    
    $ perl -e ' $a{ for (1,2,3) {} } '
    syntax error at -e line 1, near "{ for "

    $ perl -e ' print ${ } '
    syntax error at -e line 1, near "${ "
    Execution of -e aborted due to compilation errors.
    ```

  - syntax error in hashref subscript - parses 's' as s///:

    ```
    ${x->{s}}
    $x->{s}
    ```

  - wrong precedence in keys()

    `if (keys %hh != 2) { print "not" }`
    parses as: `keys(%hh != 2)`

  - quotes vs. hash lookups:

    ```
        $ perl -e '  q}} '
        # ok

        $ perl -e ' $x{ q}} } '
        Unmatched right curly bracket at -e line 1, at end of line

        $ perl -e ' $x{ q]] } '
        # ok
    ```

  - syntax error in ${@} in string
    ```
    "${@}";
    ${"};
    ```

    syntax error in autoquote: $x{s}
    parses as: $x { s/// }

  - prototypes can change during compilation

    ```
    $ perl -e ' my @x; sub z0 ($$@) { zz(99, @_) } sub zz ($$$@) { print "@_\n" } @x=(9,4,6); zz(8,5,@x); z0(8,5,@x); '
    8 5 3           # zz prototype is '$$$@'
    99 8 5 9 4 6    # zz prototype was '@' when z0 was compiled
    ```


  - resolve "filehandle" methods

    ```
    $ perl -e ' package X; open(X, ">", "x"); X->print(1234) '
    $ cat x
    1234
    $ nodejs perlito5.js -Isrc5/lib -Cjs -e ' package X; open(X, ">", "x"); X->print(1234) '
    ```


  - variable declarations in expressions

    ```
    our $Verbose ||= 0;
    our (%Cache);
    my $args = @_ or @_ = @$exports;
    ```


  - add additional variants of "for"

    ```
    # "our" with localization
    $ perl -e ' use strict; our $x = 123; for our $x (1,2,3) { 2 } print "$x\n" '
    123

    # variable in scope with localization
    $ perl -e ' use strict; my $x = 123; for $x (1,2,3) { 2 } print "$x\n" '123
    123
    ```


  - add "print", "printf", "say" special parsing - note this is related to indirect object notation

    indirect object notation

      http://lwn.net/Articles/451486/

      http://www.modernperlbooks.com/mt/2009/08/the-problems-with-indirect-object-notation.html

      http://shadow.cat/blog/matt-s-trout/indirect-but-still-fatal/

      http://perlbuzz.com/mechanix/2008/02/the-perils-of-perl-5s-indirect.html

    - not indirect object:

      ```
      $ perl -e ' package x { sub new { print "A @_\n" }}; $v = new x(123); use Data::Dumper; print Dumper $v '
      ```

    - indirect object:

      ```
      $ perl -e ' package xz { sub new { print "A @_\n" }}; $v = new xz(123); use Data::Dumper; print Dumper $v '
      ```

    - not indirect object:

      ```
      $ perl -e ' sub x { print "A @_\n" } $v = new x(123); use Data::Dumper; print Dumper $v '
      ```

    - indirect object:

      ```
      $ perl perlito5.pl -I src5/lib -Cperl5 -e '  YO { x();z } ' 
      Missing right curly or square bracket near 20 at -e line 2
      $ perl -MO=Deparse -e '  YO { x();z } ' 
      do {
          x();
          'z'
      }->YO;
      ```

    ```
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
    ```

    sbertrang++ noted this is also valid:

    ```
    print( STDERR "123" )
    ```

  - add tests for signatures: "empty" _ $ ;$

  - add test for `sub _" should be in package "main"`

    ```
    $ perl -MO=Deparse -e ' package X; sub _ { 123 } '
    package X;
    sub main::_ {
        123;
    }
    ```

  - add test for defined-or vs. `m//`  (2012/9/25 Конрад Боровски <notifications@github.com>)

    Note: fixed; see test t5/01-perlito/25-syntax-defined-or.t

    ```
    $ perl perlito5.pl -Isrc5/lib -Cast-perl5 -e ' shift // 2 '
    Number or Bareword found where operator expected

    $ perl perlito5.pl -Isrc5/lib -Cast-perl5 -e ' shift / 2 '
    Can't find string terminator '/' anywhere before EOF
    ```

  - add test for filetest operators special case:

    ```
    ' -f($file).".bak" ' should be equivalent to -f "$file.bak"
    parses as -(f($file)).".bak"
    but: ' -f ($file).".bak" '
    parses correctly
    ```

    This seems to be because there is a rule that \w followed by '(' is a function call;
    ```
    this needs more testing: ' ... and(2) '
    Test: redefine 'and', 'not' and check what works.
    TODO - "-f" without arguments
    ```

    '  $s111++ + $s222 '
    parses as  (+$s222)++

    '  $step++ < $steps '
    Can't find string terminator '>' anywhere before EOF

  - from moritz, Schwern and others at
    http://stackoverflow.com/questions/161872/hidden-features-of-perl

    you can use letters as delimiters

    ```
    $ perl -Mstrict  -wle 'print q bJet another perl hacker.b'
    Jet another perl hacker.
    ```

    Likewise you can write regular expressions:

    ```
    m xabcx
    # same as m/abc/
    ```

  - `$ perl -e ' my @things= map {  no warnings; 123 } @list; '`

    "no" not allowed in expression
    (DONE) no longer a bug, this now works in perl5.22.0

- test that "use" checks the return value of modules (the "1;" thing)

- 'our' variable is not seen:

  ```sh
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' use Data::Dumper; @x = (2..4, ";)"); print Dumper \@x '  > Main.java ; javac Main.java ; java Main
    $VAR1 = [
        2,
        3,
        4,
        chr(59) . chr(41),
    ];

    Subroutine "Perlito5::Dumper::escape_string" is not using the hash "%safe_char".
  ```    

- captured lexical is not initialized:

  ```sh
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
  ```

- test BEGIN time serialization

  ```sh
  $ perl perlito5.pl -I src5/lib -Cperl5 -e ' my ($x, $y); { $x }; my $z; @aaa = @X::xxx + $bbb; BEGIN { $aaa = [ 1 .. 5 ]; $bbb = { 5, $aaa }; $ccc = sub { my %x; 123 } } $/; my $s; BEGIN { $s = 3 } BEGIN { *ccc2 = \$ccc; } '
  ```

- fix regex delimiters, or escape the regexes

    ```
    $ perl perlito5.pl -Isrc5/lib -I. -It -Cperl5 -e ' s/\$a$a/b$b/g; tr/\$c$c/d$d/; m/\$e$e/; ' 
    ```

- compile-time eval() is not bound to the "program" environment, but to the "compiler" environment instead

  see README-perlito5-js near "Compile-time / Run-time interleaving"

  ```
  my $v;
  BEGIN { $v = "123" }
  use Module $v;  # $v is not accessible at compile-time
  ```

  Test case:

  ```
  $ cat > X.pm
  package X;
  sub import { print "args [ @_ ]\n" }
  1;
  ```

  ```
  $ perl perlito5.pl -I src5/lib -I . -Cperl5 -e ' my $v; BEGIN { $v = "xxx" } use X $v; '
  # args [ X  ]
  ```

  expected result:

  ```
  # args [ X xxx ]
  ```

- test lvalue substr()

    ```
    $ perl -e ' use Data::Dumper; sub x { $_[0] = "x"; print Dumper(\@_) }  $v = "abcdef"; x( substr($v,1,3), substr($v,1,4) ); '
    $VAR1 = [
          'x',
          'xef'
        ];
    ```

    ```
    $ perl -e ' use Data::Dumper; sub x { $_[0] = "123456"; print Dumper(\@_) }  $v = "abcdef"; x( $v, substr($v,1,3), substr($v,1,4) ); '
    $VAR1 = [
          '123456',
          '234',
          '2345'
        ];
    ```

- test lvalue ternary `?:`

   ```sh
   $ java -jar perlito5.jar -I src5/lib -I . -e ' my ($a,$b,$x,$y,$result) = 1..5; $a > $b ? $x : $y = $result; print "[$x] [$y]\n"; '
   [3] [5]
   
   $ java -jar perlito5.jar -I src5/lib -I . -e ' my ($a,$b,$x,$y,$result) = 1..5; $a = 4; $a > $b ? $x : $y = $result; print "[$x] [$y]\n"; '
   [5] [4]
   ``` 

- add tests for CORE::GLOBAL namespace

- `__DATA__` and `__END__` can be anywhere in the line

  ```sh
  $ perl -e 'print 123 __END__ x xx + '
  123

  $ perl -e 'print 123 + __END__ x xx + '
  syntax error at -e line 1, at EOF
  ```

  however:

  ```sh
  $ perl -e ' @ __END__ = (123); print @ __END__ ;'
  123
  ```


Perl6 backend
-------------

- Running the tests using perl6:

  ```sh
    # TODO - this is not implemented yet
    . util-perl6/setup-perlito5-perl6.sh
    find t5/01-perlito/*.t | perl -ne ' print "*** $_"; chomp; print ` perl perlito5.pl -I./src5/lib -Cperl6 $_ > tmp.p6 && perl6 tmp.p6  ` '
  ```


- keep comments

- context: wantarray, return-comma

    ```
        sub x { return 7, 8 }
    vs. sub x { return (7, 8) }
    ```

  use an "out-of-band" parameter to set the call context, like:

    ```
    $v = x( :scalar )   # 8
    $v = x( :list   )   # 2
    ```

- <> is lines()

- 0..$#num to @num.keys

- choose `@*ARGS` or `@_` in `shift()` and `pop()`

- typeglob assignment

- "given" statement not implemented

- refactoring sub arguments

    my $x = $_[0];
    my ($x, $y, @rest) = @_;    # check if @_ is unused elsewhere in the sub

- placeholder

    ```
    my ($a, $, $c) = 1..3;
    ($a, *, $c) = 1..3;
    ```

- `__PACKAGE__`

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

- `${^NAME}` needs curly-escaping

- `${^GLOBAL_PHASE}` is not writeable
    workaround in set_global_phase()


Compile-time execution environment
----------------------------------

- TODO - identify aliases: [[[ `BEGIN { $ccc = 3; *ccc2 = \$ccc; }` ]]] dumps:

      $main::ccc2 = $main::ccc;

  instead of:

      *main::ccc2 = \$main::ccc;

  Note: $ccc is not emitted if the value is "undef".

- TODO - lexicals are not shared


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

    ```
    $ perl -e ' my @x = $x %x '
    Operator or semicolon missing before %x at -e line 1.
    Ambiguous use of % resolved as operator % at -e line 1.
    Illegal modulus zero at -e line 1.
    ```

- no warnings 'redefine';

- source code - remove Perl 6 code such as "token"
   (fixed: This is only loaded if the grammar compiler is needed)

- `*{ $name }{CODE}->();`
    (DONE in java)

- `local(*{$caller."::a"}) = \my $a;`

- `*{$pkg . "::foo"} = \&bar;`

- `local $SIG{__WARN__};`

- bug https://github.com/fglock/Perlito/issues/10

    "Perlito 5 JS has syntax errors"

  Tried

    - YUI Compressor online
    - Google Closure Compiler
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

    ```
    $ perl -lwe '*4 = sub { print "yes" }; 4->()'
    yes
    ```

    ```
    $ perl -e ' sub x { print 123 } x->() '
    Undefined subroutine &main::1 called at -e line 1.
    123
    ```

- return value of continue-block

    ```
    $ perl -e ' sub x {  { 456 } continue { 789 } } print "@{[ x() ]}\n" '
    456 789
    ```

Deprecate
---------

- Interpreter backend
  - this is not being maintained; the code is still in src5/lib/Perlito5/Eval.pm just in case
  - to compute constant foldings use `Perlito5::FoldConstant` instead

- special backend option `_comp` dumps the compile-time execution environment:

  ```sh
  $ perl perlito5.pl -Isrc5/lib -I. -It -C_comp -e '  (0, undef, undef, @_)[1, 2] ; { 123 } sub x { 456; { 3 } }'
  ```


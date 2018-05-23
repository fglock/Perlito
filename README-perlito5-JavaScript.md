
Perlito5-in-JavaScript
======================

Perlito5-in-JavaScript - what works
--------------


- Subroutines, anonymous subroutines, prototypes, namespaces.
  `main`, `CORE` and `CORE::GLOBAL`.

- Scalar, Hash, Array, Code should work the same as in perl.

- Objects should work the same as in perl.

- Scalar vs. List context, wantarray().

- Variables.
  `my`, `our`, `local`, `state`.
  Most special variables.
  Most variable declarations.

- Most control structures.
  `next`, `last`, `redo`, `continue` and labels.
  `goto &subr`.
  `goto LABEL` - some use patterns work.

- Most string, numeric, array, hash operators.

- Some I/O operators.

- Some regex operators.

- Tie array.

- AUTOLOAD.



Perlito5-in-JavaScript differences from "perl"
--------------

- reference to scalar doesn't work properly.
    The reference points to a copy of the value

- `~~` smartmatch is not implemented

- `sleep` is not implemented in the browser.
  Perlito5 in node.js implements `sleep` using an npm module.

- Perlito5 in the browser doesn't implement many of the perl I/O operators;
  Perlito5 in node.js should implement most of the I/O operators.

- JavaScript doesn't implement reference counting;
  DESTROY will not work;
  weaken() will be a no-op.

- Overload is not implemented yet.

- XS is not supported.
  Use `JS::inline` instead.

- Variable aliasing is not implemented yet;
  modifying `$_` or `$_[0]` doesn't change the original variable.

- utf8 is not implemented yet.

- Regex is missing some features;
  `/x`, `/s`, `/e` modifiers are supported.

- Control structures are partially implemented;
  - `goto LABEL` - some use patterns work.
  - `goto &sub` works, but tail call is not implemented yet.
  - "computed goto" is not implemented
  - `given` is not implemented.

- String pos() is not implemented yet.

- Signals are not implemented yet.

- BEGIN blocks are mostly implemented.

- Typeglobs are partially implemented.

- tie() is partially implemented.

- String increment partially implemented - array and hash lookups; not in scalars.

- runtime error messages often do not include the right line number in the Perl code


Calling Perl subroutines from JavaScript
=========

- `var myfun = p5cget( "My::Module", "mysub" );`

    Export a Perl subroutine to a JavaScript variable

- `var myobj = p5cget( "My::Module", "mysub" )( [ arg1, arg2 ], null );`

    Call a Perl subroutine.
    The "null" in the end means calling in scalar context.

- `var myobj = p5call( "Statistics::Distributions", "new", [ arg1, arg2 ], null );`

    Method call.


Internals
=========


Perlito5 JavaScript Data model Overview
--------------


Perlito5 compiler globals

- context: system-wide; shared by all modules

```perl
    $Perlito5::CORE_PROTO = { 'CORE::join' => '$@' }
```

    - hashref with subroutine-full-name to prototype mappings in the CORE package only

```perl
    $Perlito5::PROTO = { 'main::mysub' => undef }
```

    - hashref with subroutine-full-name to prototype mappings

- context: module-wide

```perl
    $Perlito5::PKG_NAME = "main"
```

    - the current package name (unescaped string)

- context: subroutine-wide

```perl
    $Perlito5::THROW = 1
```

    - boolean value; tracks if the current subroutine needs to catch a javascript "throw" as a return-value

- context: lexical block

```perl
    $Perlito5::VAR = [ { '$_'    => { decl => 'our', namespace => 'main' } ]
```

    - arrayref-of-hashes with variable-short-names to (declaration type, namespace) mappings


method call
- lookup the method in the `_class_` attribute; follow the class hierarchy through @ISA until the UNIVERSAL base class.
- do a native call on the method.
- the argument list is the named array `List__` (that is, `@_`).
- additional arguments can be used to pass out-of-band data, such as: caller(), wantarray()
- the invocant is the first argument. `this` is not used.

subroutine call
- lookup the subroutine in the current namespace; follow the hierarchy until the CORE base class.
- do a native method call.
- the argument list is the named array `List__` (that is, `@_`).
- additional arguments can be used to pass out-of-band data, such as: caller(), wantarray()
- `this` is not used.

Hash
- native {} with perl5-specific getters and setters

Array
- native [] with perl5-specific getters and setters

Scalar
- native value

Tied containers
- javascript object with perl5-specific getters and setters

HashRef
- native {} wrapped in a `HashRef` object

ArrayRef
- native [] wrapped in a `ArrayRef` object

ScalarRef
- native value wrapped in a `ScalarRef` object

CodeRef
- native value

Object
- one of the reference types, with a `_class_` attribute

Class
- classes are stored in the CLASS global hash
- the `_class_` attribute points to the class itself
- classes inherit from UNIVERSAL

Namespace
- namespaces are stored in the p5pkg global hash
- current namespace object is in the PKG variable
- Namespace inherits from from CORE::GLOBAL, which inherits from CORE
- Namespace is a copy of the Class, but with a different inheritance
- Namespace and Class are updated in parallel, both when a sub is declared or when using typeglob assignment

Calling context (`wantarray`, caller)
- TODO

- these are the possible compile-time contexts:
    'scalar'
    'list'
    'void'
    'runtime' (unknown)

    'str'
    'bool'
    'num'

- at run-time the contexts are in wantarray (`p5want` variable):
    1
    0
    undef

Alias
- TODO
- using String/Boolean/Number (boxed types) as SCALARs doesn't seem to work

List (eg. subroutine return value)
- native []

eval string
- the compiler gets the current namespace as an argument
- javascript eval() happens at the current runtime lexical context

eval block
- ...

do block
- ...

AUTOLOAD
- ...

my
- ...

local
- ...

our
- ...



JavaScript resources
--------------


https://github.com/eriwen/javascript-stacktrace
- how to get a stacktrace in browsers

https://github.com/audreyt/pugs/tree/master/perl5/PIL2JS
- Pugs Perl6 in javascript

crypt implementation
https://github.com/TimDumol/unix-crypt-td-js/blob/master/src/unix-crypt-td.js

Regex
--------------

- regex extension libraries

    See: http://xregexp.com https://github.com/benekastah/empcre

- modifiers: g i m s x

- From http://www.regular-expressions.info/javascript.html

    No \A or \Z anchors to match the start or end of the string. Use a caret or dollar instead.
    Lookbehind is not supported at all. Lookahead is fully supported.
    No atomic grouping or possessive quantifiers
    No Unicode support, except for matching single characters with
    No named capturing groups. Use numbered capturing groups instead.
    No mode modifiers to set matching options within the regular expression.
    No conditionals.
    No regular expression comments with `(?#text)`



Compile-time / Run-time interleaving (TODO)
--------------

- See perlref:
    named subroutines are created at compile time so their lexical variables
    get assigned to the parent lexicals from the first execution of the parent
    block.
    If a parent scope is entered a second time, its lexicals are created again,
    while the nested subs still reference the old ones.


```bash
    $ perl -e ' use strict; { my $x = 3; sub z { 123 } BEGIN { print "$x ", z, "\n" } INIT { $x = 4 } print "$x\n" } '
     123
    3
```

    open anonymous block in the compiling environment
    add incomplete block to the AST
    add variable my $x to the AST
    add variable my $x to the compiling environment
    open named sub in the compiling environment
    add incomplete sub to the AST
    close named sub in the compiling environment
    add sub to the AST
    compile and run BEGIN block in the compiling environment
    # add BEGIN side-effects to the AST
    compile INIT block
    add INIT block to the AST
    compile print
    add print to the AST
    close anonymous block
    add block to the AST

```perl
    (sub {
        my $x = 3;
        $NAMESPACE::z = sub { 123 };  # named sub
        push @COMPILING::RUN,  sub { 1 };        # BEGIN block result
        push @COMPILING::INIT, sub { $x = 4 };   # INIT block
        push @COMPILING::RUN,  sub { print "$x\n" };
    })->();
    $_->() for @COMPILING::INIT;
    $_->() for @COMPILING::RUN;
```

```bash
    $ perl -e ' use strict; my $y = 123; sub x { my $x = 3; sub z { $y } BEGIN { print "$x ", z, "\n" } INIT { $x = 4 } print "$x\n" } '
```

```perl
    (sub {
        my $y = 123;

        (sub {
            my $x = 3;
            $NAMESPACE::z = sub { 123 };  # named sub
            1;                            # BEGIN block result
            push @COMPILING::INIT, sub { $x = 4 };   # INIT block
        })->();

        $NAMESPACE::x = sub {
            my $x = 3;
            1;                            # BEGIN block result
            print "$x\n";
        };

    })->();
    $_->() for @COMPILING::INIT;
    $_->() for @COMPILING::RUN;
```


- disambiguation between block and hash should not backtrack, because any internal special blocks would be compiled/run twice

- anonymous blocks, named subroutines and variables must be instantiated at compile-time



Reference counting (TODO)
--------------

- add a new attribute `_cnt_` to all references
- lexicals, locals decrement the count when going out of scope
- call DESTROY when count reaches zero



Cell-based aliasing (TODO)
--------------


- slow
- allows aliasing (rw parameters to functions)
- allows `tie`, because collection access is done through methods
- simplifies autovivification
- allows lvalue subroutines, such as chop(), chomp(), keys(), pos(), substr(), undef()
- allows `our`

- examples:

```javascript
    v = new Cell();
    v.set(5);
    f(v);   // f gets a copy of the cell; v.set() inside f() modifies the original variable.
    1 + v;  // calls v.valueOf()
    x = v;  // alias (copies the cell); v.set() modifies x.valueOf()
    x.set( v.valueOf() );  // copies the value (doesn't alias)

    h.lookup("x");  // looks up h["x"] for a cell; autovivifies if needed
    v.lookup("x");  // error if the cell in v contains something else than undef or an arrayref
```

- see `mp6_Scalar` class in src6/lib/Perlito/Python/Runtime.py


Tail call (TODO)
--------------

- a tail call can be transformed into a loop at the caller:

```perl
    sub mysub { goto &other }
```

  can be called:

```perl
    ret = new TailCall(mysub);
    do {
        ret = ret.f();
    }
    while (ret instanceof TailCall);
```

- alternately, the loop can be run at the subroutine itself, but this creates other problems


- tailcalls
    same-subroutine tailcalls could execute a `redo` in the current subroutine.


"js3" virtual machine
--------------


- "js3" milestones (TODO list)

    - timely destruction (depends on reference counting); weaken()

    - tie()

    - Overload

    - variable aliasing (`$_[0]`, for-loop, map)

    - make perlito usable for CPAN smoke tests:

        node perlito5.js Makefile.PL
        make test


- possible implementation of lexical variables

This allows better control over memory allocation (for example, to implement destructors and aliasing)

```javascript
    // lexical variables
    function p5env_001 () {};
    var p5env = p5env_001;
    p5env.a = 3;

    function myfun () {
        var p5env_002 = function () {};
        p5env_002.prototype = p5env_001;

        var p5env = new p5env_002();
        p5env.b = 4;
        process.stdout.write( ""  + p5env.a + " " + p5env.b + "\n" );
        p5env.a = 5;
        process.stdout.write( ""  + p5env.a + " " + p5env.b + "\n" );
    }

    myfun();
    process.stdout.write( ""  + p5env.a + " " + p5env.b + "\n" );

    @_ is special:
    $_[n] lvalue can be represented by

    at_env[n][at_var[n]] = ...
```

subroutine call:

```javascript
    mysub( [
            env,         "var", // a variable
            env.arr,     0,     // a subscript
            [ 123 ],     0      // a value
        ], context );
```

lvalue subroutine call:

    ??? - maybe use 'context' to force return of a settable object

tied containers:
Tie::Scalar magic can be implemented with getters/setters in env

```javascript
    p5env.b = 4;    // call b setter if there is a setter
```

Tie::Array, Tie::Hash

```javascript
    p5env.list_b[0] = 4; // ??? - but this works if we use a method to get/set the variable
```

problem: tie'ing a variable in the outer scope (`p5env_001`) using defineProperty() would be
inherited by the inner scope (`p5env_002` in the example above).

workaround: access variables from the outer scope directly, without using inheritance.
This can be complicated by statements like { my $v = 0 if $x }
which create lexicals dynamically - but the behaviour in this case is undefined anyway.

defineProperty() can be used to provide accessors to non-tied containers:

```javascript
    Object.defineProperty( Array.prototype, "p5aget", {
        enumerable : false,
        value : function (i) { return this[i] }
    });
    Object.defineProperty( Array.prototype, "p5aset", {
        enumerable : false,
        value : function (i, v) { this[i] = v; return this[i] }
    });

    Object.defineProperty( Object.prototype, "p5hget", {
        enumerable : false,
        value : function (i) { return this[i] }
    });
    Object.defineProperty( Object.prototype, "p5hset", {
        enumerable : false,
        value : function (i, v) { this[i] = v; return this[i] }
    });

    b = [5,6,8];
    b.p5aset(2, 13);
    process.stdout.write( " " + b.p5aget(2) + "\n" );

    h = { x : 4, y : 7 };
    h.p5hset("x", 13);
    process.stdout.write( " " + h.p5hget("x") + "\n" );
```

- Alternative implementation for lvalue `@_` and tail calls

  ```javascript
    // calling function x()
    // the variables (a,b,c) are lexicals aliased to $_[0], $_[1], $_[2]
    // .mod signals that @_ was modified
    // .at  returns @_
    // .res is the funtion result
    // .tail is a tail-call result flag
    function(){
        r = x([a,b,c], want);
        if (r.mod) { a=r.at[0]; b=r.at[1]; c=r.at[2] };
        while (r.tail) {
            // do tail calls
        }
        return r.res
    }()
  ```



Perlito5 JavaScript backend TODO list
=====================================


Features
--------

- create `__DATA__`

  `%Perlito5::DATA_SECTION` contains the `__DATA__` for each package

- DESTROY

  Try::Tiny uses DESTROY to implement finally() - and it doesn't execute in js:

  ```
    $ nodejs perlito5.js -Isrc5/lib -I. -I /usr/local/lib/perl5/site_perl/5.20.0  -e ' use Try::Tiny; try { print "this\n" }; try { die "this" } catch { print "catched\n" } finally { print "done\n" } '
    this
    catched

    $ perl -e ' use Try::Tiny;  try { print "this\n" }; try { die "this" } catch { print "catched\n" } finally { print "done\n" } '
    this
    catched
    done
  ```

- constant subroutines
- prototype mismatch

  ```
    $ perl -e ' sub X () { 123 } print X, "\n"; eval " sub X { 456 } "; '
    123
    Prototype mismatch: sub main::X () vs none at (eval 1) line 1.
    Constant subroutine X redefined at (eval 1) line 1.
  ```

- reference to scalar doesn't work

  ```
    $ node perlito5.js -Isrc5/lib -I.  -e ' use Data::Dumper; $v = [20, \$v ]; print Dumper ($v) '
    $VAR1 = [
            20,
            \undef,
        ];
  ```

- assign old-self to my / local

  ```
    local $Some_Global = $Some_Global;
  ```

- missing some types of subroutine signatures

- AUTOLOAD() called from UNIVERSAL autovivifies packages

    add tests

- delete() in the middle of an array turns exists() off:

  ```
    $ perl -e ' @a = (3..7); delete $a[2]; print "exists ", (exists $a[$_] ? 1 : 0), "\n" for 0 .. $#a '
    exists 1
    exists 1
    exists 0
    exists 1
    exists 1
  ```

- delete() in src5/lib/Perlito5/Grammar/String.pm doesn't seem to work:

  ```
    delete($quote_flags->{$flag_to_reset});
    delete($quote_flags->{last_flag});
  ```

- `~~` operator not implemented; See also `when` implementation
- `given` statement not implemented
- `when` should use a `break` exception inside `given`, and a `next` exception inside `for`.
- `default` statement not implemented

- javascript errors don't show in the global error handler when running in node.js

- "autoload" the compiler if eval-string or require() are used (eval-string needs the compiler at run-time)

  https://github.com/fglock/Perlito/issues/23

- symbol variables like `$] ${"main::\$"} $#_`
- check that `@_`, `$_`, `$a`, `$b` and other special variables are in the right context (lexical, global, package global)

- add alternate mro's
- cache the mro

- add regex compiler
- support all perl5 regex syntax
- `@v = /x/g`

- regex variables localization in blocks
    $ perl -e ' "a" =~ /(.)/; print $1; { "b" =~ /(.)/; print $1; } print $1, "\n"; '
    aba
    $ perl -e ' "a" =~ /(.)/; print $1; { "b" =~ //; print $1; } print $1, "\n"; '
    abb
    $ perl -e ' "a" =~ /(.)/; print $1; { "b" =~ /x/; print $1; } print $1, "\n"; '
    aaa

- some qr() and quotemeta() details

  ```
    $ perl -e ' my $x = qr/ \A x /x; my $y = qr/$x y \Q[z]/; use Data::Dumper; print Dumper $x; print Dumper $y; '
    $VAR1 = qr/(?x-ism: \A x )/;
    $VAR1 = qr/(?-xism:(?x-ism: \A x ) y \[z\])/;

    $ perl -e ' print " a b \Q [ a \nn"; '
     a b \ \[\ a\ \
    n

    $ perl -e ' print "x\Q[\Qx]\Ex\n" '
    x\[x\\\]x\          # '\' is quoted, but 'Q' disappears
  ```

- qr() returns a Regexp object

  ```
    {
        package Regexp;
        sub x { 123 }
    }
    $a = qr//;
    print $a->x, "\n";  # 123
  ```

- lvalue ternary: `($a_or_b ? $a : $b) = $c;`
- lvalue substr()
- 4-arguments substr()
- lvalue pos($str)
- pos($str)
- lvalue chomp(), chop()
- lvalue subroutine

- bug: variable aliases create copies instead

  ```
    for (@x) { $_++ }   # doesn't change @x
  ```

- generate more compact code; maybe use more subroutines instead of inlining;

  autovivification is probably the most verbose part of the code.

  Use less `throw` - it is not (yet?) optimized by V8

- in the browser: implement `use` with XMLHttpRequest (what are the security implications?)

- aliasing between lexicals and globals

  ```
    $ perl -e 'use strict; my $x = 3; *main::z = \$x; print $main::z; '
    3
  ```

- finish `overload` implementation

  See: p5str

- pack(), unpack()

- flip-flop operator

  if either operand to scalar '..' is a constant the value is implicitly compared to the input line number ($.)

- caller() in nodejs and Chrome:

  See: https://github.com/stacktracejs

  ```
  $ nodejs perlito5.js -Isrc5/lib -I. -e ' sub x { print JS::inline("new Error().stack") }; sub yy { x() }; my $f = sub { yy() }; $f->() '

  "Error
    at tmp104 [as x] (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:10:63)
    at tmp105 [as yy] (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:18:26)
    at tmp106 (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:27:28)
    at eval (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:32:7)
    ..."
  ```

- die() details

  ```
  If the output is empty and $@ already contains a value (typically
  from a previous eval) that value is reused after appending
  "\t...propagated". This is useful for propagating exceptions:

      eval { ... };
      die unless $@ =~ /Expected exception/;

  If the output is empty and $@ contains an object reference that
  has a "PROPAGATE" method, that method will be called with
  additional file and line number parameters. The return value
  replaces the value in $@; i.e., as if "$@ = eval {
  $@->PROPAGATE(__FILE__, __LINE__) };" were called.
  ```

- `my sub x {...}`


Implemented but missing more tests
----------------------------------

- add tests: `my` variables - this doesn't work as expected: `my $v = $v`

- add tests using closures, to check that the redeclared variable is a different variable

  ```
    $ perl   -e '  my $x = 10; print "$x\n"; my $x; print "$x\n"; '
    10
    [space]
    $ nodejs perlito5.js -Isrc5/lib  -e '  my $x = 10; print "$x\n"; my $x; print "$x\n"; '
    10
    10
  ```

- /e modifier

- lvalue `$#a`

- local

  add tests:  exiting a block with 'last' doesn't retrieve previous 'local' values:

  ```
    $ nodejs perlito5.js -Isrc5/lib -I. -It  -e ' $_ = "abc"; /(b)/; print "$1\n"; { print "$1\n"; /(c)/; print "$1\n"; last; } print "$1\n"; '
    b
    b
    c
    c
  ```

- `&` prototype

  add tests; see Try::Tiny

- add symbol tables for scalar, array and hash

- references to typeglobs:

  ```
    $ perl -e ' print ref(\*main) '
    GLOB
  ```

- prototype() can be set by aliasing:

  `*x = sub ($$) { 123 }; *y = *x;  print prototype(\&y)   # $$`

- `or` has SCALAR context (Abigail++):

  See: t5/01-perlito/23-eval.t

- 'next', 'last' in expression

  ```
  (*{"${callpkg}::$sym"} = \&{"${pkg}::$sym"}, next)
  ... ) and last
  ```

- check that `\(@a)` and `\@a` have different meanings

- 'x' in list context

  `@{$cache}{@$ok} = (1) x @$ok;`

- `while () {}`

  this is implemented - it now needs some tests:

  http://blogs.perl.org/users/peter_martini/2014/05/spelunking-why-while-is-my-new-favorite-perl-ism.html

  ```
  while () {}     # infinite loop - while(1)
  while (()) {}   # no loop
  ```

- pass `@_` to `&sub`

  ```
  $ node perlito5.js -I./src5/lib -Cjs -e ' @_ = (1,2);  &foo; '
  # call foo(@_)
  ```



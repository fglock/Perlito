Perlito5 Javascript backend TODO list
=====================================

Performance
-----------

    compilation time (bootstrap) slowed down during the year 2015,
    from about 10s to 30s.

    $ cat misc/git_bisect_slowjs.pl
    use strict;
    use Data::Dumper;
    `make build-5js`;
    my @v = `make boot-5js 2>&1`;
    my ($time) = grep { /^user\s+(\d)m(\d+)\./ } @v;
    my @time = $time =~ /(\d)m(\d+)\./;
    my $t = $time[0] * 60 + $time[1];
    print STDERR "t = $t\n";
    # good 0
    # bad  1
    # skip 125
    exit(1) if $t > 20;
    exit(125) if $t > 3;
    exit(0);

    $ git bisect start origin/master ec254b20f4e39de9ace6a7cf032087b845dee8bc
    $ git bisect run perl misc/git_bisect_slowjs.pl

    # t = 26s
    5d337a844c706cd1759b3c55666d070658ae1c85 is the first bad commit

    # t = 20s
    ff9201b480dba0d594e71029298fb0e07d982b60 is the first bad commit

    # t = 17s
    bb97211c066848ef417c15fe030dc483f81fc986 is the first bad commit

Features
--------

-- DESTROY
    Try::Tiny uses DESTROY to implement finally() - and it doesn't execute in js:

    $ nodejs perlito5.js -Isrc5/lib -I. -I /usr/local/lib/perl5/site_perl/5.20.0  -e ' use Try::Tiny; try { print "this\n" }; try { die "this" } catch { print "catched\n" } finally { print "done\n" } ' 
    this
    catched
    
    $ perl -e ' use Try::Tiny;  try { print "this\n" }; try { die "this" } catch { print "catched\n" } finally { print "done\n" } ' 
    this
    catched
    done

-- constant subroutines
-- prototype mismatch
    $ perl -e ' sub X () { 123 } print X, "\n"; eval " sub X { 456 } "; '
    123
    Prototype mismatch: sub main::X () vs none at (eval 1) line 1.
    Constant subroutine X redefined at (eval 1) line 1.

-- reference to scalar doesn't work
    $ node perlito5.js -Isrc5/lib -I.  -e ' use Data::Dumper; $v = [20, \$v ]; print Dumper ($v) '
    $VAR1 = [
            20,
            \undef,
        ];

-- phase order
    print x();          # js will try to execute this before the sub declaration
    sub x { "ok\n"; }
    #  TypeError: Object [object Object] has no method 'x'

-- assign old-self to my / local
    local $Some_Global = $Some_Global;

-- missing some types of subroutine signatures

-- AUTOLOAD() called from UNIVERSAL autovivifies packages
    add tests

-- delete() in the middle of an array turns exists() off:

    $ perl -e ' @a = (3..7); delete $a[2]; print "exists ", (exists $a[$_] ? 1 : 0), "\n" for 0 .. $#a '
    exists 1
    exists 1
    exists 0
    exists 1
    exists 1

-- delete() in src5/lib/Perlito5/Grammar/String.pm doesn't seem to work:
    delete($quote_flags->{$flag_to_reset});
    delete($quote_flags->{last_flag});

-- "~~" operator not implemented; See also "when" implementation
-- "given" statement not implemented
-- "when" should use a "break" exception inside "given", and a "next" exception inside "for".
-- "default" statement not implemented

-- javascript errors don't show in the global error handler when running in node.js

-- "autoload" the compiler if eval-string or require() are used (eval-string needs the compiler at run-time)
    https://github.com/fglock/Perlito/issues/23

-- symbol variables like $] ${"main::\$"} $#_
-- check that @_, $_, $a, $b and other special variables are in the right context (lexical, global, package global)

-- add alternate mro's
-- cache the mro

-- add regex compiler
-- /x modifier
-- support all perl5 regex syntax
-- @v = /x/g

-- regex variables localization in blocks
    $ perl -e ' "a" =~ /(.)/; print $1; { "b" =~ /(.)/; print $1; } print $1, "\n"; '
    aba
    $ perl -e ' "a" =~ /(.)/; print $1; { "b" =~ //; print $1; } print $1, "\n"; '
    abb
    $ perl -e ' "a" =~ /(.)/; print $1; { "b" =~ /x/; print $1; } print $1, "\n"; '
    aaa

-- some qr() and quotemeta() details

    $ perl -e ' my $x = qr/ \A x /x; my $y = qr/$x y \Q[z]/; use Data::Dumper; print Dumper $x; print Dumper $y; '
    $VAR1 = qr/(?x-ism: \A x )/;
    $VAR1 = qr/(?-xism:(?x-ism: \A x ) y \[z\])/;

    $ perl -e ' print " a b \Q [ a \nn"; '
     a b \ \[\ a\ \
    n

    $ perl -e ' print "x\Q[\Qx]\Ex\n" '
    x\[x\\\]x\          # '\' is quoted, but 'Q' disappears

-- qr() returns a Regexp object
    {
        package Regexp;
        sub x { 123 }
    }
    $a = qr//;
    print $a->x, "\n";  # 123

-- bug: variable redeclaration does not work
-- javascript "var" erases the outer value within the whole current lexical scope
-- bug: "my" variables - this doesn't work as expected: my $v = $v
   possible fix: rename variables
   possible fix: initialize variables to null
-- add tests using closures, to check that the redeclared variable is a different variable
    $ perl   -e '  my $x = 10; print "$x\n"; my $x; print "$x\n"; '
    10
    [space]
    $ nodejs perlito5.js -Isrc5/lib  -e '  my $x = 10; print "$x\n"; my $x; print "$x\n"; '
    10
    10

-- lvalue ternary: ($a_or_b ? $a : $b) = $c;
-- lvalue substr()
-- 4-arguments substr()
-- lvalue pos($str)
-- pos($str)
-- lvalue chomp(), chop()
-- lvalue subroutine

-- bug: variable aliases create copies instead
    for (@x) { $_++ }   # doesn't change @x

-- generate more compact code; maybe use more subroutines instead of inlining;
   autovivification is probably the most verbose part of the code.
   Use less "throw" - it is not (yet?) optimized by V8

-- in the browser: implement "use" with XMLHttpRequest (what are the security implications?)

-- aliasing between lexicals and globals
    $ perl -e 'use strict; my $x = 3; *main::z = \$x; print $main::z; '
    3

-- finish "overload" implementation
    See: p5str

-- pack(), unpack()

-- BEGIN{} should execute in the environment of the program under compilation
-- BEGIN/END that are defined inside blocks/closures need to run inside some pseudo-environment
    even if the closure was never created or used in the first place

-- flip-flop operator
    if either operand to scalar '..' is a constant the value is implicitly compared to the input line number ($.)

-- caller() in nodejs and Chrome:

    $ nodejs perlito5.js -Isrc5/lib -I. -e ' sub x { print JS::inline("new Error().stack") }; sub yy { x() }; my $f = sub { yy() }; $f->() '

  "Error
    at tmp104 [as x] (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:10:63)
    at tmp105 [as yy] (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:18:26)
    at tmp106 (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:27:28)
    at eval (eval at <anonymous> (/perlito5.js:31586:57), <anonymous>:32:7)
    ..."

-- die() details

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


Implemented but missing more tests
----------------------------------

-- /e modifier

-- lvalue $#a

-- local
   add tests:  exiting a block with 'last' doesn't retrieve previous 'local' values:
    $ nodejs perlito5.js -Isrc5/lib -I. -It  -e ' $_ = "abc"; /(b)/; print "$1\n"; { print "$1\n"; /(c)/; print "$1\n"; last; } print "$1\n"; '
    b
    b
    c
    c

-- '&' prototype
    add tests; see Try::Tiny

-- add symbol tables for scalar, array and hash
-- references to typeglobs:
    $ perl -e ' print ref(\*main) '
    GLOB

-- prototype() can be set by aliasing:
    *x = sub ($$) { 123 }; *y = *x;  print prototype(\&y)   # $$

-- "or" has SCALAR context (Abigail++):
    See: t5/01-perlito/23-eval.t

-- 'next', 'last' in expression
    (*{"${callpkg}::$sym"} = \&{"${pkg}::$sym"}, next)
    ... ) and last

-- check that \(@a) and \@a have different meanings

-- 'x' in list context
    @{$cache}{@$ok} = (1) x @$ok;

-- while () {}
    this is implemented - it now needs some tests:
    # http://blogs.perl.org/users/peter_martini/2014/05/spelunking-why-while-is-my-new-favorite-perl-ism.html
    while () {}     # infinite loop - while(1)
    while (()) {}   # no loop

-- pass @_ to &sub
    $ node perlito5.js -I./src5/lib -Cjs -e ' @_ = (1,2);  &foo; '
    # call foo(@_)



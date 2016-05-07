"Perlito6" Perl 6 compiler
==========================

    This is Perlito6, a compiler that implements a subset of Perl 6.

Backends
--------

    Perlito6 can compile Perl 6 programs into one of the 'backend' 
    languages:

        Perl 5, JavaScript, Python 2.6

    Previous versions of Perlito6 also compile to: Go, Common Lisp, and Ruby 1.9

Web
---

    Main Perlito repository: http://github.com/fglock/Perlito

    Main Project web page: http://fglock.github.io/Perlito

    Run Perlito6 online, in the browser: http://fglock.github.io/Perlito/perlito/perlito6.html

Source code
-----------

    The source code for Perlito can be found in the 'src' directory.
    Perlito6 is written in Perl 6.

USING PERLITO6
==============

Using the JavaScript backend in the browser
-------------------------------------------

Open the file 'html/perlito6.html' in a browser (Firefox, Chrome, Safari).

Alternately, browse to:

[http://fglock.github.io/Perlito/perlito/perlito6.html](http://fglock.github.io/Perlito/perlito/perlito6.html)

Using the JavaScript backend in the command line
------------------------------------------------

    export PERL5LIB=lib5
    perl perlito6.pl -Cjs -e 'class Main { say "hello, World!" }' > tmp.js
    d8 tmp.js

Creating a bootstrapped compiler:

    # compile src6/util/perlito6.pl to JavaScript, using perlito6.pl:
    export PERL5LIB=lib5
    perl perlito6.pl -Cjs src6/util/perlito6.pl > perlito6.js

    # testing the bootstrap with the v8 developer shell
    d8 perlito6.js -- -Cjs src6/util/perlito6.pl > perlito-new.js

Using the Perl 5 backend
------------------------

From CPAN:

  Perlito is available at [http://search.cpan.org/dist/v6/lib/v6.pm](http://search.cpan.org/dist/v6/lib/v6.pm)

Installation:

    cpan v6

From the Perlito git project directory:

    export PERL5LIB=lib5
    perl perlito6.pl -Cperl5 -e 'class Main { say "hello, World!" }' | perl

Creating the bootstrapped compiler:

    # compile src6/util/perlito6.pl using perlito6.pl:
    export PERL5LIB=lib5
    perl perlito6.pl -Cperl5 src6/util/perlito6.pl > perlito-perl5.pl 

Alternately:

    make build-6to5

Using the Python backend
------------------------

    # initialize "./libpy" and compile "perlito6.py"
    make build-6py

    # using the perlito6.py compiler
    export PYTHONPATH=libpy
    python perlito6.py -Cpython -e 'class Main { say "hello, World!" }' | python 

Using the Ruby backend
----------------------

    export RUBYLIB=src6/lib

PRODUCING AST OUTPUT
====================

    perl perlito6.pl -Cast-perl6 -e 'class Main { say "hello, World!" }'

RUNNING TESTS
=============

    Perl 5:

    find t6/*.t | perl -ne ' print "*** $_"; chomp; print ` perl perlito6.pl -Cperl5 $_ | perl ` '

    JavaScript:

    find t6/*.t | perl -ne ' print "*** $_"; chomp; print ` d8 perlito6.js -- -Cjs $_ > tmp.js && d8 tmp.js ` '

    Python 2.6:

    find t6/*.t | perl -ne ' print "*** $_"; chomp; print ` python perlito6.py -Cpython $_ | python ` '

AUTHORS
=======

Flavio Soibelmann Glock - fglock@gmail.com
The Pugs Team - perl6-compiler@perl.org

SEE ALSO
========

The Perl 6 homepage at http://dev.perl.org/perl6
The Pugs homepage at http://pugscode.org
The Perlito homepage at http://fglock.github.io/Perlito

COPYRIGHT
=========

Copyright 2006, 2009, 2010, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html



Perlito6 TODO list
======================

CPAN distribution
-----------------

- split into 2: v6, Perlito6

- create Markdown files for github documentation;
- example: http://www.unexpected-vortices.com/sw/rippledoc/quick-markdown-example.html
- in CPAN, convert all the documentation to POD using one of these:

    $ perl -e ' use Markdown::To::POD "markdown_to_pod"; my @text = <>; my $pod = markdown_to_pod(join "", @text); print $pod; ' README

    $ perl -e ' use Markdown::Pod;my $m2p = Markdown::Pod->new; my @text = <>; my $pod = $m2p->markdown_to_pod(markdown => join "", @text); print $pod; ' README


Perlito6 namespace
--------------

- move all internal packages inside Perlito6 namespace


Eval.pm module
--------------

- add exceptions

- complete ast nodes implementation


Command line compiler (src6/util/perlito6.pl)
--------------

- build Ast cache using JSON (we currently use Perl5 Data::Dumper) or XML (Go has XML and JSON input)

- option to build binaries (Lisp, Java, Go) or modules (Perl, Lisp, JavaScript)

- add "make" capabilities (test file dates, etc)

- does it need a config file? (lib location, make details)

- add '-B' option (execute)


Perlito6 in JavaScript
--------------

- reuse the good parts of Perlito5 data model

- add node.js features
-- properly implement die(); this should avoid some infinite-loops when we keep compiling after an error is found


Perlito6 in Rakudo/Niecza/Pugs
--------------

- any Perl6 should be able to execute Perlito6 directly. What do we need to fix?

-- rakudo:

    =begin must be followed by an identifier; (did you mean "=begin pod"?)
    at src6/lib/Perlito6/Emitter/Token.pm:331

-- rakudo: use "augment" to add new methods to classes

    [13:20] <jnthn> r: use MONKEY_TYPING; class Foo { ... }; augment class Foo { }


Perlito6 in Lisp
--------------

- test other Lisp implementations


Perlito6 in Go
--------------

- document which release of Go to use


Perlito6 in Perl5
--------------

(no issues at the moment)


Perlito6 in Parrot
--------------

- finish OO, class variables

- fix die() parameter handling


Perlito6 in Python
--------------

- module loading uses mangled filenames (with underlines); it should use dot-separated names instead.


Perlito6 in Ruby
--------------

- module loading uses mangled filenames (with underlines); it should use dot-separated names instead.


Missing Backends
--------------

- Haskell

- Clojure


Missing Features
--------------

- detailed syntax errors

- type annotations - FIXED

- 'use v5' is not supported
  (maybe not needed for Perlito)

- no 'state', 'constant', 'local'

- debugger

- chain operators - supported by the grammar, but no AST representation

- "loop(;;)"

- "when"


Missing Features of the Token sub-compiler
--------------

- no quantifiers - FIXED

- no variable interpolation

- use the new precedence parser


Nice to Have
--------------

- 'perlito-format' script (see gofmt and perltidy)


Tests
--------------

- run some tests from the standard test suite

- use Test.pm - FIXED


Grammar
--------------

- item-assignment precedence is different from list-assignment

- captures like: / <a> <a>* / - the first capture must share storage with the second capture

- modify the grammar to return multi-line strings as multiple strings
  (this makes it easier to write a beautifier script)

- parse Namespaces as array of string (we are going to split them anyway)

- double quote variable interpolation - FIXED (without method calls)

- and expression interpolation 

- the grammar should be aware of function arity


Semantics
--------------

- @a.values and %a.keys return Array
  These should return a List 

- Not implemented: %a = (list) 


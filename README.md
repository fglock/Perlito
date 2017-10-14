"Perlito5" Perl to Java compiler and Perl to JavaScript compiler
=======================

This is Perlito, a compiler collection that implements Perl 5 and Perl 6 programming languages.

Compile Perl to Java
---------------------

- compile Perl 5 to Java source code

- run Perl 5 directly in the JVM

Compile Perl to JavaScript
--------------------------

- compile Perl 5 to JavaScript source code; run Perl 5 directly in the browser or nodejs

- compile Perl 6 to JavaScript source code; run Perl 6 directly in the browser or nodejs

Compile Perl to other backends
------------------------------

Perlito can also compile Perl 5 or Perl 6 programs into one of these languages:

- compile Perl 5 to Perl 5

- compile Perl 6 to Perl 5

- compile Perl 6 to Python 2.6

The following compilers are work in progress - some tests pass, some tests fail.
Check the Changelog file for details:

- compile Perl 5 to Perl 6

- compile Perl 6 to Ruby 1.9

- compile Perl 6 to Go

- compile Perl 6 to Common Lisp (SBCL)


Web
---

Main Perlito repository: http://github.com/fglock/Perlito

Main Project web page: http://fglock.github.io/Perlito

Run Perlito online, in the browser:

  - [http://fglock.github.io/Perlito/perlito/perlito5.html](http://fglock.github.io/Perlito/perlito/perlito5.html)

  - [http://fglock.github.io/Perlito/perlito/perlito6.html](http://fglock.github.io/Perlito/perlito/perlito6.html)
  
CPAN distribution:

  - http://search.cpan.org/dist/v6/lib/v6.pm

  - http://search.cpan.org/dist/Perlito5/lib/Perlito5.pm

Source code
-----------

The source code for Perlito5 can be found in the 'src5' directory.
Perlito5 is written mostly in Perl 5 (the grammar uses its own mini-language.)

The source code for Perlito6 can be found in the 'src6' directory.
Perlito6 is written in Perl 6.

See Also
--------

  - [README-perlito6](README-perlito6.md)

  - [README-perlito5](README-perlito5.md)

      - [README-perlito5-JavaScript](README-perlito5-JavaScript.md)

      - [README-perlito5-Java](README-perlito5-Java.md)

  - [README-howto-release](README-howto-release.md)

Build Status
------------

[![Build Status](https://travis-ci.org/fglock/Perlito.svg)](https://travis-ci.org/fglock/Perlito)


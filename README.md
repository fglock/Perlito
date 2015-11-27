"Perlito" Perl compiler
=======================

This is Perlito, a compiler collection that implements a subset of Perl 5 and Perl 6.

Backends
--------

Perlito can compile Perl 5 or Perl 6 programs into one of the 'backend'
languages:

  - Perl 5 to Perl 5
  - Perl 5 to Javascript

  - Perl 6 to Perl 5
  - Perl 6 to Javascript
  - Perl 6 to Python 2.6

The following compilers are work in progress - some tests pass, some tests fail.
Check the Changelog file for details:

  - Perl 5 to Perl 6
  - Perl 5 to Java 6
  - Perl 6 to Ruby 1.9
  - Perl 6 to Go
  - Perl 6 to Common Lisp (SBCL)
  - Perl 6 to Ruby 1.9

Web
---

Main Perlito repository: http://github.com/fglock/Perlito

Main Project web page: http://fglock.github.io/Perlito

Run Perlito online, in the browser:

  - [http://fglock.github.io/Perlito/perlito/perlito5.html](http://fglock.github.io/Perlito/perlito/perlito5.html)

  - [http://fglock.github.io/Perlito/perlito/perlito6.html](http://fglock.github.io/Perlito/perlito/perlito6.html)

Source code
-----------

The source code for Perlito5 can be found in the 'src5' directory.
Perlito5 is written mostly in Perl 5 (the grammar uses its own mini-language.)

The source code for Perlito6 can be found in the 'src6' directory.
Perlito6 is written in Perl 6.

SEE ALSO
--------

  - [README-perlito6](README-perlito6)

  - [README-perlito5](README-perlito5)


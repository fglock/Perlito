"Perlito" release to CPAN
=========================

Perlito5 Release
----------------

* run tests

    make test

* increment version number in "src5/lib/Perlito5.pm"

    example: "$VERSION = '9.017';"

* update "Changelog" header

    example: "9.017 cpan:Perlito5  2016-01-19"

* go to "cpan-Perlito5" directory and execute "make-Perlito5.sh"

    cd cpan-Perlito5

    . make-Perlito5.sh

* build and test the module inside "cpan-Perlito5" directory

    perl Makefile.PL
    make
    make test

* make dist

* publish to pause.perl.org

* commit and push the changes

    example: "Perlito5 - CPAN - v9.017"

* release perlito5.jar and perlito5.js

  See https://github.com/fglock/Perlito/releases


Perlito6 Release
----------------

* run tests

    make test-6to5

* increment version number in "cpan-v6/v6.pm"

    example: "$v6::VERSION = '0.045';"

* update "Changelog" header

    example: "0.045 cpan:v6  2016-01-19"

* go to "cpan-v6" directory and execute "make-v6.sh"

    cd cpan-v6

    . make-v6.sh

* build and test the module inside "cpan-v6" directory

    perl Makefile.PL
    make
    make test

* make dist

* publish to pause.perl.org

TODO for the release process
----------------------------

* BUG - "cpan-v6/ChangeLog" is not updated automatically

* BUG - the README files are not included in the cpan distribution


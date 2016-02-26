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


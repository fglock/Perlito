set -x
export PERL6LIB=./lib6

mkdir lib6
mkdir lib6/Perlito5
mkdir lib6/Perlito5/Perl6

cp src5/lib/Perlito5/Perl6/Runtime.pm lib6/Perlito5/Perl6/Runtime.pm

perl -Ilib5 perlito5.pl -Cperl6 src5/lib/Perlito5/Test.pm  >  lib6/Perlito5/Test.pm


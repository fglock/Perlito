[ -d cpan-Perlito5 ] && cd cpan-Perlito5

rm -rf lib
rm -rf t
rm -rf scripts

touch META.yml

mkdir t
cp -r ../t5/*.t t/
rm t/21-test.t

mkdir lib
cp -r ../lib5/Perlito5 lib/

mkdir scripts
cp ../src5/util/perlito5.pl scripts/perlito5

perldoc -otext scripts/perlito5 > README

# perldoc -opod scripts/perlito5 > lib/Perlito5.pm
# echo '1;' >> lib/Perlito5.pm

perl Makefile.PL


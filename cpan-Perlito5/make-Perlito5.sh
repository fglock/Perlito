[ -d cpan-Perlito5 ] && cd cpan-Perlito5

rm -rf lib
rm -rf t
rm -rf scripts

touch META.yml

mkdir t
cp -r ../t5/*.t t/
rm t/21-test.t
rm t/22-unicode.t

cp -r ../lib5 lib
rm -rf lib/Perlito

# perldoc -otext v6.pm > README
cp ../README README

mkdir scripts
cp ../perlito5.pl scripts/perlito5

perl Makefile.PL


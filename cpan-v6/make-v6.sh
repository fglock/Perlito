[ -d cpan-v6 ] && cd cpan-v6

rm -rf lib
rm -rf t

touch META.yml

mkdir t
cp ../t6/*.t t/
perl -pi -e 's/use v6;/use v6-perlito;/' t/*.t

rm -rf t/bugs
cp -r ../lib5 lib
rm -rf lib/Perlito5

cp ../src6/lib/Perlito6/Python/Prelude.pm lib/Perlito6/Python/Prelude.pm
cp ../src6/lib/Perlito6/Ruby/Runtime.rb   lib/Perlito6/Ruby/Runtime.rb

perldoc -otext v6.pm > README

mkdir lib/v6
cp v6.pm lib/
cp perlito.pm lib/v6/

rm -rf scripts
mkdir scripts
cp ../perlito6.pl scripts/perlito6

perl Makefile.PL


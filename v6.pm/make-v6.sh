[ -d v6.pm ] && cd v6.pm

rm -rf lib
rm -rf t

touch META.yml

cp -r ../t .
perl -pi -e 's/use v6;/use v6-perlito;/' t/*.t

rm -rf t/bugs
cp -r ../lib5 lib

cp ../src6/lib/Perlito/Python/Prelude.pm lib/Perlito/Python/Prelude.pm
cp ../src6/lib/Perlito/Ruby/Runtime.rb   lib/Perlito/Ruby/Runtime.rb

perldoc -otext v6.pm > README

mkdir lib/v6
cp v6.pm lib/
cp perlito.pm lib/v6/

rm -rf scripts
mkdir scripts
cp ../perlito6.pl scripts/perlito6

perl Makefile.PL


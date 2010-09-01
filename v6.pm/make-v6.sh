rm -rf lib
rm -rf t

touch META.yml

cp -r ../t .
perl -pi -e 's/use v6;/use v6-perlito;/' t/*.t

rm -rf t/bugs
cp -r ../lib5 lib

perldoc -otext v6.pm > README

mkdir lib/v6
cp v6.pm lib/
cp perlito.pm lib/v6/


rm -rf lib
rm -rf t

touch META.yml

cp -r ../t .
perl -pi -e 's/use v6;/use v6-perlito; no warnings;/' t/*.t

rm -rf t/bugs
cp -r ../lib5 lib
perl -pi -e 's/use strict;/use strict; no warnings;/' \
  lib/MiniPerl6/*.pm lib/MiniPerl6/Grammar/*.pm lib/MiniPerl6/Perl5/*.pm lib/MiniPerl6/Emitter/*.pm
rm lib/Test.pm
rm t/21-test.t

perldoc -otext v6.pm > README

mkdir lib/v6
cp v6.pm lib/
cp perlito.pm lib/v6/


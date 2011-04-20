rm -rf lib5-new
rm -rf lib5-old
rm -rf lib5-tmp

echo 'Compiling with old version'

perl util-perl5/make.pl
perl perlito.pl -Cperl5 util/perlito.pl > perlito-new.pl
rm -rf lib5-old
mv lib5 lib5-old
rm -rf lib5
mv lib5-new lib5
rm perlito.pl
mv perlito-new.pl perlito.pl

echo 'Compiling with new version'

perl util-perl5/make.pl
perl perlito.pl -Cperl5 util/perlito.pl > perlito-new.pl
rm -rf lib5-tmp
mv lib5 lib5-tmp
mv lib5-new lib5

echo 'Finished. Old version moved to lib5-old. " diff -r lib5 lib5-tmp "  should show no differences'


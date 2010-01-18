rm -rf lib5-new
rm -rf lib5-old
rm -rf lib5-tmp

echo 'Compiling with old version'
rm -rf libast-perl5
perl util-perl5/make.pl
mv lib5 lib5-old
mv lib5-new lib5

echo 'Compiling with new version'
rm -rf libast-perl5
perl util-perl5/make.pl -Cast-perl5
perl util-perl5/make.pl
mv lib5 lib5-tmp
mv lib5-new lib5

echo 'Finished. Old version moved to lib5-old. " diff -r lib5 lib5-tmp "  should show no differences'


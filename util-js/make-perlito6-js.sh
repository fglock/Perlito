
rm tmp.js
perl -Ilib5 perlito6.pl --verbose -Cjs src6/util/perlito6-browser.pl > tmp.js
rm perlito-old.js
mv html/perlito6.js perlito-old.js
mv tmp.js html/perlito6.js


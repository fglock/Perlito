
rm tmp.js
perl -Ilib5 perlito.pl --verbose -Cjs src/util/perlito-browser.pl > tmp.js
rm perlito-old.js
mv html/perlito.js perlito-old.js
mv tmp.js html/perlito.js


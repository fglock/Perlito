
rm tmp.js
perl -Ilib5 perlito.pl --verbose -Cjs util-js/perlito.pl > tmp.js
rm perlito-old.js
mv perlito.js perlito-old.js
mv tmp.js perlito.js


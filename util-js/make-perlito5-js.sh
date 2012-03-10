#!/bin/sh
rm tmp.js
perl -Ilib5 perlito5.pl --verbose -Cjs src5/util/perlito5-browser.pl > tmp.js
rm perlito5-old.js
mv html/perlito5.js perlito5-old.js
mv tmp.js html/perlito5.js


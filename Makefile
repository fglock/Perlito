# default actions

all : build-5to5 build-5js build-5browser

test-all : test-5to5 test-5js

test : test-5js

boot-all : boot-5to5 boot-5js build-5browser


# more

minify ::
	jsmin < html/perlito5.js > html/perlito5m.js

build-5to5 ::
	perl perlito5.pl -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && cp perlito5-new.pl perlito5.pl

build-5js ::
	perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js

build-5browser ::
	perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5-browser.pl > html/perlito5.js

boot-5js ::
	time node perlito5.js -Isrc5/lib -Cjs src5/util/perlito5.pl > perlito5-new.js && diff perlito5-new.js perlito5.js ; cp perlito5-new.js perlito5.js

test-5js ::
	prove -r -e 'node perlito5.js -I./src5/lib' t5

boot-5to5 ::
	time perl perlito5.pl -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && diff perlito5-new.pl perlito5.pl ; cp perlito5-new.pl perlito5.pl

test-5to5 ::
	prove -r -e 'perl perlito5.pl -I./src5/lib ' t5

test-5to6 ::
	-find t5/*/*.t | perl -ne ' chomp; print "*** perl6 $$_.p6$$/"; chomp; print `perl -Ilib5 perlito5.pl -Cperl6 $$_ > $$_.p6 && perl6 $$_.p6 `'

.PHONY: test-5to6

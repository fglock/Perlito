
# default actions

all : build-5to5 build-5js build-5java build-5browser build-5to6browser

test-all : test-5to5 test-5js

test : test-5js

boot-all : boot-5to5 boot-5js build-5browser


# Perl 6

# TODO - convert this test to use 'prove'
test-6to5 ::
	set -x
	find t6/*.t | perl -ne ' chomp; print "# $$_\n" . ` perl -I./lib5 perlito6.pl -Cperl5 $$_ | perl -I./lib5 ` '

build-6browser ::
	perl -Ilib5 perlito6.pl -Cjs src6/util/perlito6-browser.pl > docs/perlito/perlito6.js

build-6to5 ::
	makefiles/build-6to5.sh

build-6py ::
	makefiles/build-6py.sh

# Perl 5

minify ::
	jsmin < docs/perlito/perlito5.js > docs/perlito/perlito5.min.js
	jsmin < docs/perlito/perlito6.js > docs/perlito/perlito6.min.js

build-5to5 ::
	perl perlito5.pl --bootstrapping -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && cp perlito5-new.pl perlito5.pl

build-5js ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js

build-5java ::
	makefiles/make_perlito5-lib-jar.sh

build-5browser ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs src5/util/perlito5-browser.pl > docs/perlito/perlito5.js

build-5to6browser ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs src5/util/perlito5-browser-perl6.pl > docs/perlito/perlito5to6.js

build-5js3 ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs3 src5/util/perlito5.pl > perlito5.js

boot-5js ::
	time nodejs perlito5.js --bootstrapping -Isrc5/lib -Cjs src5/util/perlito5.pl > perlito5-new.js && diff perlito5-new.js perlito5.js ; cp perlito5-new.js perlito5.js

boot-5to5 ::
	time perl perlito5.pl --bootstrapping -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && diff perlito5-new.pl perlito5.pl ; cp perlito5-new.pl perlito5.pl

# Perl 5 tests (all tests must pass)

test-5js ::
	perl makefiles/copy-tests.pl t5/Test-summary-report-5js.txt t5-js
	prove -r -e 'nodejs perlito5.js -I./src5/lib -I./t ' t5-js

test-5jar ::
	perl makefiles/copy-tests.pl t5/Test-summary-report-5jar.txt t5-jar
	prove -r -e 'java -jar perlito5.jar -I src5/lib -I t ' t5-jar

test-5to5 ::
	perl makefiles/copy-tests.pl t5/Test-summary-report-5to5.txt t5-5to5
	prove -r -e 'perl perlito5.pl -I./src5/lib ' t5-5to5

test-5js-parallel ::
	perl makefiles/copy-tests.pl t5/Test-summary-report-5js.txt t5-js
	prove -j 3 -r -e 'nodejs perlito5.js -I./src5/lib -I./t ' t5-js

test-5jar-parallel ::
	perl makefiles/copy-tests.pl t5/Test-summary-report-5jar.txt t5-jar
	prove -j 3 -r -e 'java -jar perlito5.jar -I src5/lib -I t ' t5-jar

# Perl 5 dev tests (some tests can fail)

test-5js-dev ::
	prove -r -e 'nodejs perlito5.js -I./src5/lib -I./t ' t5

test-5js-parallel-dev ::
	prove -j 3 -r -e 'nodejs perlito5.js -I./src5/lib -I./t ' t5

test-5java-dev ::
	prove -r -e 'perl makefiles/run_java_test.pl' t5

test-5jar-dev ::
	prove -r -e 'java -jar perlito5.jar -I src5/lib -I t ' t5

test-5javaunit-dev ::
	prove -r -e 'perl makefiles/run_java_test.pl' t5/unit

test-5to5-dev ::
	prove -r -e 'perl perlito5.pl -I./src5/lib ' t5

test-5to6-dev ::
	-find t5/*/*.t | perl -ne ' chomp; print "*** perl6 $$_.p6$$/"; chomp; print `perl -Ilib5 perlito5.pl -Cperl6 $$_ > $$_.p6 && perl6 $$_.p6 `'

.PHONY: test-5to6

.PHONY: clean

clean ::
	-rm *.class
	-rm *.js
	-rm *.py
	-rm *.pyc
	-rm misc/Java/*.class
	-rm *.[0-9][0-9]
	-rm -rf t/
	-rm -rf t5-js/
	-rm -rf t5-jar/
	-rm -rf t5-5to5/
	-rm -rf org/perlito/Perlito5/*.class

# :set tabstop=4
# :set noexpandtab	  " Use tabs, not spaces


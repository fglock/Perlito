
PERL5_INC := -I./lib5


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
	rm -rf libpy/
 
	mkdir libpy
	touch libpy/__init__.py
   
	cp src6/lib/Perlito6/Python/Runtime.py libpy/Perlito6__Python__Runtime.py
   
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/AST.pm			 > libpy/Perlito6__AST.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Python/Prelude.pm  > libpy/Perlito6__Python__Prelude.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Precedence.pm	  > libpy/Perlito6__Precedence.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Expression.pm	  > libpy/Perlito6__Expression.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Macro.pm		   > libpy/Perlito6__Macro.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Test.pm			> libpy/Perlito6__Test.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Grammar.pm		 > libpy/Perlito6__Grammar.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Grammar/Control.pm > libpy/Perlito6__Grammar__Control.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Grammar/Regex.pm   > libpy/Perlito6__Grammar__Regex.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Emitter/Token.pm   > libpy/Perlito6__Emitter__Token.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Eval.pm			> libpy/Perlito6__Eval.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Runtime.pm		 > libpy/Perlito6__Runtime.py
   
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/JavaScript/Emitter.pm > libpy/Perlito6__JavaScript__Emitter.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Lisp/Emitter.pm	> libpy/Perlito6__Lisp__Emitter.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Perl5/Emitter.pm   > libpy/Perlito6__Perl5__Emitter.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Go/Emitter.pm	  > libpy/Perlito6__Go__Emitter.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Parrot/Emitter.pm  > libpy/Perlito6__Parrot__Emitter.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Python/Emitter.pm  > libpy/Perlito6__Python__Emitter.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Ruby/Emitter.pm	> libpy/Perlito6__Ruby__Emitter.py
	perl ${PERL5_INC} perlito6.pl -Cpython src6/lib/Perlito6/Java/Emitter.pm	> libpy/Perlito6__Java__Emitter.py
   
	perl ${PERL5_INC} perlito6.pl -Cpython src6/util/perlito6.pl					> ./perlito6.py
   

# Perl 5

minify ::
	jsmin < docs/perlito/perlito5.js > docs/perlito/perlito5.min.js
	jsmin < docs/perlito/perlito6.js > docs/perlito/perlito6.min.js

build-5to5 ::
	perl perlito5.pl --bootstrapping -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && cp perlito5-new.pl perlito5.pl

build-5js ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js

build-5java ::
	. ./make_perlito5-lib-jar.sh

build-5browser ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs src5/util/perlito5-browser.pl > docs/perlito/perlito5.js

build-5to6browser ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs src5/util/perlito5-browser-perl6.pl > docs/perlito/perlito5to6.js

build-5js3 ::
	perl perlito5.pl --bootstrapping -I./src5/lib -Cjs3 src5/util/perlito5.pl > perlito5.js

boot-5js ::
	time nodejs perlito5.js --bootstrapping -Isrc5/lib -Cjs src5/util/perlito5.pl > perlito5-new.js && diff perlito5-new.js perlito5.js ; cp perlito5-new.js perlito5.js

test-5js ::
	prove -r -e 'nodejs perlito5.js -I./src5/lib -I./t ' t5

test-5js-parallel ::
	prove -j 9 -r -e 'nodejs perlito5.js -I./src5/lib -I./t ' t5

test-5java ::
	prove -r -e 'perl run_java_test.pl' t5

test-5jar ::
	prove -r -e 'java -jar perlito5.jar -I src5/lib -I t ' t5

test-5javaunit ::
	prove -r -e 'perl run_java_test.pl' t5/unit

boot-5to5 ::
	time perl perlito5.pl --bootstrapping -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && diff perlito5-new.pl perlito5.pl ; cp perlito5-new.pl perlito5.pl

test-5to5 ::
	prove -r -e 'perl perlito5.pl -I./src5/lib ' t5

test-5to6 ::
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

# :set tabstop=4
# :set noexpandtab	  " Use tabs, not spaces


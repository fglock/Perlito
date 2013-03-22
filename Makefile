# default actions

all : build-5to5 build-5js build-5browser

test-all : test-5to5 test-5js

test : test-5js

boot-all : boot-5to5 boot-5js build-5browser


# Perl 6

# TODO - convert this test to use 'prove'
test-6to5 ::
	set -x
	find t6/*.t | perl -ne ' chomp; print "# $$_\n" . ` perl -I./lib5 perlito6.pl -Cperl5 $$_ | perl -I./lib5 ` '

build-6to5 ::
	set -x

	# rm -rf lib5-new/Perlito6
	rm -rf lib5-new
	
	mkdir lib5-new
	mkdir lib5-new/Perlito6
	mkdir lib5-new/Perlito6/Clojure
	mkdir lib5-new/Perlito6/Emitter
	mkdir lib5-new/Perlito6/Go
	mkdir lib5-new/Perlito6/Grammar
	mkdir lib5-new/Perlito6/Java
	mkdir lib5-new/Perlito6/Javascript
	mkdir lib5-new/Perlito6/Lisp
	mkdir lib5-new/Perlito6/Parrot
	mkdir lib5-new/Perlito6/Perl5
	mkdir lib5-new/Perlito6/Python
	mkdir lib5-new/Perlito6/Rakudo
	mkdir lib5-new/Perlito6/Ruby
	
	cp src6/lib/Perlito6/Perl5/Runtime.pm lib5-new/Perlito6/Perl5/Runtime.pm
	
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Test.pm			  > lib5-new/Perlito6/Test.pm
	
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/AST.pm			  > lib5-new/Perlito6/AST.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Emitter/Token.pm   > lib5-new/Perlito6/Emitter/Token.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Eval.pm			  > lib5-new/Perlito6/Eval.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Expression.pm	  > lib5-new/Perlito6/Expression.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Grammar.pm		  > lib5-new/Perlito6/Grammar.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Grammar/Control.pm > lib5-new/Perlito6/Grammar/Control.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Grammar/Regex.pm   > lib5-new/Perlito6/Grammar/Regex.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Macro.pm			  > lib5-new/Perlito6/Macro.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Precedence.pm	  > lib5-new/Perlito6/Precedence.pm
	
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Go/Emitter.pm	  > lib5-new/Perlito6/Go/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Java/Emitter.pm	  > lib5-new/Perlito6/Java/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Javascript/Emitter.pm > lib5-new/Perlito6/Javascript/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Lisp/Emitter.pm	  > lib5-new/Perlito6/Lisp/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Parrot/Emitter.pm  > lib5-new/Perlito6/Parrot/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Python/Emitter.pm  > lib5-new/Perlito6/Python/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Ruby/Emitter.pm	  > lib5-new/Perlito6/Ruby/Emitter.pm
	
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Perl5/Emitter.pm   > lib5-new/Perlito6/Perl5/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Perl5/Prelude.pm   > lib5-new/Perlito6/Perl5/Prelude.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Runtime.pm		  > lib5-new/Perlito6/Runtime.pm
	
	perl -I./lib5 perlito6.pl -Cperl5 src6/util/perlito6.pl				  > ./perlito6-new.pl
	
	# other files we use for cross-compilation
	
	cp src6/lib/Perlito6/Javascript/Runtime.js lib5-new/Perlito6/Javascript/Runtime.js
	cp src6/lib/Perlito6/Python/Runtime.py	   lib5-new/Perlito6/Python/Runtime.py
	
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Javascript/Prelude.pm   > lib5-new/Perlito6/Javascript/Prelude.pm
	
	# older backends we want to keep around for now
	
	cp src6/lib/Perlito6/Go/Runtime.go		   lib5-new/Perlito6/Go/Runtime.go
	cp src6/lib/Perlito6/Lisp/Runtime.lisp	   lib5-new/Perlito6/Lisp/Runtime.lisp

	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Clojure/Emitter.pm  > lib5-new/Perlito6/Clojure/Emitter.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Go/Prelude.pm	   > lib5-new/Perlito6/Go/Prelude.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Lisp/Prelude.pm	   > lib5-new/Perlito6/Lisp/Prelude.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Parrot/Match.pm	   > lib5-new/Perlito6/Parrot/Match.pm
	perl -I./lib5 perlito6.pl -Cperl5 src6/lib/Perlito6/Rakudo/Emitter.pm   > lib5-new/Perlito6/Rakudo/Emitter.pm

	# clean up

	rm -rf lib5-old/Perlito6
	mv lib5/Perlito6 lib5-old/Perlito6
	mv lib5-new/Perlito6 lib5/Perlito6

	rm perlito6-old.pl
	mv perlito6.pl perlito6-old.pl
	mv perlito6-new.pl perlito6.pl


# Perl 5

minify ::
	jsmin < html/perlito5.js > html/perlito5m.js

build-5to5 ::
	perl perlito5.pl -Isrc5/lib -Cperl5 src5/util/perlito5.pl > perlito5-new.pl && cp perlito5-new.pl perlito5.pl

build-5js ::
	perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5.pl > perlito5.js

build-5browser ::
	perl perlito5.pl -I./src5/lib -Cjs src5/util/perlito5-browser.pl > html/perlito5.js

build-5js3 ::
	perl perlito5.pl -I./src5/lib -Cjs3 src5/util/perlito5.pl > perlito5.js

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

# :set tabstop=4
# :set noexpandtab	  " Use tabs, not spaces



export PERL5_INC='-I./lib5'

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
	mkdir lib5-new/Perlito6/JavaScript
	mkdir lib5-new/Perlito6/Lisp
	mkdir lib5-new/Perlito6/Parrot
	mkdir lib5-new/Perlito6/Perl5
	mkdir lib5-new/Perlito6/Python
	mkdir lib5-new/Perlito6/Rakudo
	mkdir lib5-new/Perlito6/Ruby
	
	cp src6/lib/Perlito6/Perl5/Runtime.pm lib5-new/Perlito6/Perl5/Runtime.pm
	
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Test.pm			  > lib5-new/Perlito6/Test.pm
	
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/AST.pm			  > lib5-new/Perlito6/AST.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Emitter/Token.pm   > lib5-new/Perlito6/Emitter/Token.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Eval.pm			  > lib5-new/Perlito6/Eval.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Expression.pm	  > lib5-new/Perlito6/Expression.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Grammar.pm		  > lib5-new/Perlito6/Grammar.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Grammar/Control.pm > lib5-new/Perlito6/Grammar/Control.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Grammar/Regex.pm   > lib5-new/Perlito6/Grammar/Regex.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Macro.pm			  > lib5-new/Perlito6/Macro.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Precedence.pm	  > lib5-new/Perlito6/Precedence.pm
	
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Go/Emitter.pm	  > lib5-new/Perlito6/Go/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Java/Emitter.pm	  > lib5-new/Perlito6/Java/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/JavaScript/Emitter.pm > lib5-new/Perlito6/JavaScript/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Lisp/Emitter.pm	  > lib5-new/Perlito6/Lisp/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Parrot/Emitter.pm  > lib5-new/Perlito6/Parrot/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Python/Emitter.pm  > lib5-new/Perlito6/Python/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Ruby/Emitter.pm	  > lib5-new/Perlito6/Ruby/Emitter.pm
	
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Perl5/Emitter.pm   > lib5-new/Perlito6/Perl5/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Perl5/Prelude.pm   > lib5-new/Perlito6/Perl5/Prelude.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Runtime.pm		  > lib5-new/Perlito6/Runtime.pm
	
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/util/perlito6.pl				  > ./perlito6-new.pl
	
	# other files we use for cross-compilation
	
	cp src6/lib/Perlito6/JavaScript/Runtime.js lib5-new/Perlito6/JavaScript/Runtime.js
	cp src6/lib/Perlito6/Python/Runtime.py	   lib5-new/Perlito6/Python/Runtime.py
	
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/JavaScript/Prelude.pm   > lib5-new/Perlito6/JavaScript/Prelude.pm
	
	# older backends we want to keep around for now
	
	cp src6/lib/Perlito6/Go/Runtime.go		   lib5-new/Perlito6/Go/Runtime.go
	cp src6/lib/Perlito6/Lisp/Runtime.lisp	   lib5-new/Perlito6/Lisp/Runtime.lisp

	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Clojure/Emitter.pm  > lib5-new/Perlito6/Clojure/Emitter.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Go/Prelude.pm	   > lib5-new/Perlito6/Go/Prelude.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Lisp/Prelude.pm	   > lib5-new/Perlito6/Lisp/Prelude.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Parrot/Match.pm	   > lib5-new/Perlito6/Parrot/Match.pm
	perl ${PERL5_INC} perlito6.pl -Cperl5 src6/lib/Perlito6/Rakudo/Emitter.pm   > lib5-new/Perlito6/Rakudo/Emitter.pm

	# clean up

	mkdir lib5-old
	rm -rf lib5-old/Perlito6
	mv lib5/Perlito6 lib5-old/Perlito6
	mv lib5-new/Perlito6 lib5/Perlito6

	rm perlito6-old.pl
	mv perlito6.pl perlito6-old.pl
	mv perlito6-new.pl perlito6.pl


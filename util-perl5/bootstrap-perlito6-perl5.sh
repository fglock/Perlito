set -x
export PERL5LIB=./lib5

rm -rf lib5-new/Perlito

mkdir lib5-new
mkdir lib5-new/Perlito
mkdir lib5-new/Perlito/Clojure
mkdir lib5-new/Perlito/Emitter
mkdir lib5-new/Perlito/Go
mkdir lib5-new/Perlito/Grammar
mkdir lib5-new/Perlito/Java
mkdir lib5-new/Perlito/Javascript
mkdir lib5-new/Perlito/Lisp
mkdir lib5-new/Perlito/Parrot
mkdir lib5-new/Perlito/Perl5
mkdir lib5-new/Perlito/Python
mkdir lib5-new/Perlito/Rakudo
mkdir lib5-new/Perlito/Ruby

cp src6/lib/Perlito/Perl5/Runtime.pm lib5-new/Perlito/Perl5/Runtime.pm

perl perlito6.pl -Cperl5 src6/lib/Perlito/Test.pm            > lib5-new/Perlito/Test.pm

perl perlito6.pl -Cperl5 src6/lib/Perlito/AST.pm             > lib5-new/Perlito/AST.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Emitter/Token.pm   > lib5-new/Perlito/Emitter/Token.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Eval.pm            > lib5-new/Perlito/Eval.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Expression.pm      > lib5-new/Perlito/Expression.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Grammar.pm         > lib5-new/Perlito/Grammar.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Grammar/Control.pm > lib5-new/Perlito/Grammar/Control.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Grammar/Regex.pm   > lib5-new/Perlito/Grammar/Regex.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Macro.pm           > lib5-new/Perlito/Macro.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Precedence.pm      > lib5-new/Perlito/Precedence.pm

perl perlito6.pl -Cperl5 src6/lib/Perlito/Go/Emitter.pm      > lib5-new/Perlito/Go/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Java/Emitter.pm    > lib5-new/Perlito/Java/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Javascript/Emitter.pm > lib5-new/Perlito/Javascript/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Lisp/Emitter.pm    > lib5-new/Perlito/Lisp/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Parrot/Emitter.pm  > lib5-new/Perlito/Parrot/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Python/Emitter.pm  > lib5-new/Perlito/Python/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Ruby/Emitter.pm    > lib5-new/Perlito/Ruby/Emitter.pm

perl perlito6.pl -Cperl5 src6/lib/Perlito/Perl5/Emitter.pm   > lib5-new/Perlito/Perl5/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Perl5/Prelude.pm   > lib5-new/Perlito/Perl5/Prelude.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Runtime.pm         > lib5-new/Perlito/Runtime.pm

perl perlito6.pl -Cperl5 src6/util/perlito6.pl                > ./perlito-new.pl

# other files we use for cross-compilation

cp src6/lib/Perlito/Javascript/Runtime.js lib5-new/Perlito/Javascript/Runtime.js
cp src6/lib/Perlito/Python/Runtime.py     lib5-new/Perlito/Python/Runtime.py

perl perlito6.pl -Cperl5 src6/lib/Perlito/Javascript/Prelude.pm   > lib5-new/Perlito/Javascript/Prelude.pm

# older backends we want to keep around for now

cp src6/lib/Perlito/Go/Runtime.go         lib5-new/Perlito/Go/Runtime.go
cp src6/lib/Perlito/Lisp/Runtime.lisp     lib5-new/Perlito/Lisp/Runtime.lisp

perl perlito6.pl -Cperl5 src6/lib/Perlito/Clojure/Emitter.pm  > lib5-new/Perlito/Clojure/Emitter.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Go/Prelude.pm       > lib5-new/Perlito/Go/Prelude.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Lisp/Prelude.pm     > lib5-new/Perlito/Lisp/Prelude.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Parrot/Match.pm     > lib5-new/Perlito/Parrot/Match.pm
perl perlito6.pl -Cperl5 src6/lib/Perlito/Rakudo/Emitter.pm   > lib5-new/Perlito/Rakudo/Emitter.pm

# clean up

rm -rf lib5-old/Perlito
mv lib5/Perlito lib5-old/Perlito
mv lib5-new/Perlito lib5/Perlito

rm perlito-old.pl
mv perlito6.pl perlito-old.pl
mv perlito-new.pl perlito6.pl



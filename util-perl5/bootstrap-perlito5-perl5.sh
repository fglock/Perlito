set -x
export PERL5LIB=./lib5

rm -rf lib5-new/Perlito5

mkdir lib5-new
mkdir lib5-new/Perlito5
mkdir lib5-new/Perlito5/Clojure
mkdir lib5-new/Perlito5/Emitter
mkdir lib5-new/Perlito5/Go
mkdir lib5-new/Perlito5/Grammar
mkdir lib5-new/Perlito5/Java
mkdir lib5-new/Perlito5/Javascript
mkdir lib5-new/Perlito5/Lisp
mkdir lib5-new/Perlito5/Parrot
mkdir lib5-new/Perlito5/Perl5
mkdir lib5-new/Perlito5/Python
mkdir lib5-new/Perlito5/Rakudo
mkdir lib5-new/Perlito5/Ruby

cp src5/lib/Perlito5/Perl5/Runtime.pm lib5-new/Perlito5/Perl5/Runtime.pm

perl perlito5.pl -Cperl5 src5/lib/Perlito5/Test.pm            > lib5-new/Perlito5/Test.pm

perl perlito5.pl -Cperl5 src5/lib/Perlito5/AST.pm             > lib5-new/Perlito5/AST.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Emitter/Token.pm   > lib5-new/Perlito5/Emitter/Token.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Eval.pm            > lib5-new/Perlito5/Eval.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Expression.pm      > lib5-new/Perlito5/Expression.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Grammar.pm         > lib5-new/Perlito5/Grammar.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Grammar/Control.pm > lib5-new/Perlito5/Grammar/Control.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Grammar/Regex.pm   > lib5-new/Perlito5/Grammar/Regex.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Macro.pm           > lib5-new/Perlito5/Macro.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Precedence.pm      > lib5-new/Perlito5/Precedence.pm

perl perlito5.pl -Cperl5 src5/lib/Perlito5/Go/Emitter.pm      > lib5-new/Perlito5/Go/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Java/Emitter.pm    > lib5-new/Perlito5/Java/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Javascript/Emitter.pm > lib5-new/Perlito5/Javascript/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Lisp/Emitter.pm    > lib5-new/Perlito5/Lisp/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Parrot/Emitter.pm  > lib5-new/Perlito5/Parrot/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Python/Emitter.pm  > lib5-new/Perlito5/Python/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Ruby/Emitter.pm    > lib5-new/Perlito5/Ruby/Emitter.pm

perl perlito5.pl -Cperl5 src5/lib/Perlito5/Perl5/Emitter.pm   > lib5-new/Perlito5/Perl5/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Perl5/Prelude.pm   > lib5-new/Perlito5/Perl5/Prelude.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Runtime.pm         > lib5-new/Perlito5/Runtime.pm

perl perlito5.pl -Cperl5 src5/util/perlito5.pl                > ./perlito5-new.pl

# other files we use for cross-compilation

cp src5/lib/Perlito5/Javascript/Runtime.js lib5-new/Perlito5/Javascript/Runtime.js
cp src5/lib/Perlito5/Python/Runtime.py     lib5-new/Perlito5/Python/Runtime.py

perl perlito5.pl -Cperl5 src5/lib/Perlito5/Javascript/Prelude.pm   > lib5-new/Perlito5/Javascript/Prelude.pm

# older backends we want to keep around for now

cp src5/lib/Perlito5/Go/Runtime.go         lib5-new/Perlito5/Go/Runtime.go
cp src5/lib/Perlito5/Lisp/Runtime.lisp     lib5-new/Perlito5/Lisp/Runtime.lisp

perl perlito5.pl -Cperl5 src5/lib/Perlito5/Clojure/Emitter.pm  > lib5-new/Perlito5/Clojure/Emitter.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Go/Prelude.pm       > lib5-new/Perlito5/Go/Prelude.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Lisp/Prelude.pm     > lib5-new/Perlito5/Lisp/Prelude.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Parrot/Match.pm     > lib5-new/Perlito5/Parrot/Match.pm
perl perlito5.pl -Cperl5 src5/lib/Perlito5/Rakudo/Emitter.pm   > lib5-new/Perlito5/Rakudo/Emitter.pm

# clean up

rm -rf lib5-old/Perlito5
rm -rf lib5/Perlito5
mkdir lib5-old
mv lib5/Perlito5     lib5-old/Perlito5
mv lib5-new/Perlito5 lib5/Perlito5

rm perlito5-old.pl
mv perlito5.pl     perlito5-old.pl
mv perlito5-new.pl perlito5.pl



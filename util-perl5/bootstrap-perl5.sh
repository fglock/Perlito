export PERL5LIB=./lib5

rm -rf lib5-new

mkdir lib5-new
mkdir lib5-new/Perlito
mkdir lib5-new/Perlito/Grammar
mkdir lib5-new/Perlito/Emitter
mkdir lib5-new/Perlito/Javascript
mkdir lib5-new/Perlito/Lisp
mkdir lib5-new/Perlito/Perl5
mkdir lib5-new/Perlito/Go
mkdir lib5-new/Perlito/Parrot
mkdir lib5-new/Perlito/Python
mkdir lib5-new/Perlito/Ruby
mkdir lib5-new/Perlito/Clojure
mkdir lib5-new/Perlito/Rakudo

cp lib/Perlito/Perl5/Runtime.pm lib5-new/Perlito/Perl5/Runtime.pm

perl perlito.pl -Cperl5 lib/Perlito/Test.pm            > lib5-new/Perlito/Test.pm

perl perlito.pl -Cperl5 lib/Perlito/Grammar.pm         > lib5-new/Perlito/Grammar.pm
perl perlito.pl -Cperl5 lib/Perlito/Grammar/Control.pm > lib5-new/Perlito/Grammar/Control.pm
perl perlito.pl -Cperl5 lib/Perlito/Grammar/Regex.pm   > lib5-new/Perlito/Grammar/Regex.pm
perl perlito.pl -Cperl5 lib/Perlito/Emitter/Token.pm   > lib5-new/Perlito/Emitter/Token.pm
perl perlito.pl -Cperl5 lib/Perlito/Macro.pm           > lib5-new/Perlito/Macro.pm
perl perlito.pl -Cperl5 lib/Perlito/Expression.pm      > lib5-new/Perlito/Expression.pm
perl perlito.pl -Cperl5 lib/Perlito/Precedence.pm      > lib5-new/Perlito/Precedence.pm
perl perlito.pl -Cperl5 lib/Perlito/Eval.pm            > lib5-new/Perlito/Eval.pm

perl perlito.pl -Cperl5 lib/Perlito/Javascript/Emitter.pm > lib5-new/Perlito/Javascript/Emitter.pm
perl perlito.pl -Cperl5 lib/Perlito/Lisp/Emitter.pm    > lib5-new/Perlito/Lisp/Emitter.pm
perl perlito.pl -Cperl5 lib/Perlito/Go/Emitter.pm      > lib5-new/Perlito/Go/Emitter.pm
perl perlito.pl -Cperl5 lib/Perlito/Parrot/Emitter.pm  > lib5-new/Perlito/Parrot/Emitter.pm
perl perlito.pl -Cperl5 lib/Perlito/Python/Emitter.pm  > lib5-new/Perlito/Python/Emitter.pm
perl perlito.pl -Cperl5 lib/Perlito/Ruby/Emitter.pm    > lib5-new/Perlito/Ruby/Emitter.pm

perl perlito.pl -Cperl5 lib/Perlito/Perl5/Emitter.pm   > lib5-new/Perlito/Perl5/Emitter.pm
perl perlito.pl -Cperl5 lib/Perlito/Perl5/Prelude.pm   > lib5-new/Perlito/Perl5/Prelude.pm

perl perlito.pl -Cperl5 util/perlito.pl                > ./perlito-new.pl

# other files we use for cross-compilation

cp lib/Perlito/Javascript/Runtime.js lib5-new/Perlito/Javascript/Runtime.js
cp lib/Perlito/Python/Runtime.py     lib5-new/Perlito/Python/Runtime.py

perl perlito.pl -Cperl5 lib/Perlito/Javascript/Prelude.pm   > lib5-new/Perlito/Javascript/Prelude.pm

# older backends we want to keep around for now

cp lib/Perlito/Go/Runtime.go         lib5-new/Perlito/Go/Runtime.go
cp lib/Perlito/Lisp/Runtime.lisp     lib5-new/Perlito/Lisp/Runtime.lisp

perl perlito.pl -Cperl5 lib/Perlito/Clojure/Emitter.pm  > lib5-new/Perlito/Clojure/Emitter.pm
perl perlito.pl -Cperl5 lib/Perlito/Go/Prelude.pm       > lib5-new/Perlito/Go/Prelude.pm
perl perlito.pl -Cperl5 lib/Perlito/Lisp/Prelude.pm     > lib5-new/Perlito/Lisp/Prelude.pm
perl perlito.pl -Cperl5 lib/Perlito/Parrot/Match.pm     > lib5-new/Perlito/Parrot/Match.pm
perl perlito.pl -Cperl5 lib/Perlito/Rakudo/Emitter.pm   > lib5-new/Perlito/Rakudo/Emitter.pm

# clean up

rm -rf lib5-old/
mv lib5 lib5-old
mv lib5-new lib5

rm perlito-old.pl
mv perlito.pl perlito-old.pl
mv perlito-new.pl perlito.pl



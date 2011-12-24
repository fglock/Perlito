
export PERL5LIB=lib5

rm -rf libpy/

mkdir libpy
touch libpy/__init__.py

cp src6/lib/Perlito/Python/Runtime.py libpy/Perlito__Python__Runtime.py

perl perlito6.pl -Cpython src6/lib/Perlito/AST.pm             > libpy/Perlito__AST.py
perl perlito6.pl -Cpython src6/lib/Perlito/Python/Prelude.pm  > libpy/Perlito__Python__Prelude.py
perl perlito6.pl -Cpython src6/lib/Perlito/Precedence.pm      > libpy/Perlito__Precedence.py
perl perlito6.pl -Cpython src6/lib/Perlito/Expression.pm      > libpy/Perlito__Expression.py
perl perlito6.pl -Cpython src6/lib/Perlito/Macro.pm           > libpy/Perlito__Macro.py
perl perlito6.pl -Cpython src6/lib/Perlito/Test.pm            > libpy/Perlito__Test.py
perl perlito6.pl -Cpython src6/lib/Perlito/Grammar.pm         > libpy/Perlito__Grammar.py
perl perlito6.pl -Cpython src6/lib/Perlito/Grammar/Control.pm > libpy/Perlito__Grammar__Control.py
perl perlito6.pl -Cpython src6/lib/Perlito/Grammar/Regex.pm   > libpy/Perlito__Grammar__Regex.py
perl perlito6.pl -Cpython src6/lib/Perlito/Emitter/Token.pm   > libpy/Perlito__Emitter__Token.py
perl perlito6.pl -Cpython src6/lib/Perlito/Eval.pm            > libpy/Perlito__Eval.py
perl perlito6.pl -Cpython src6/lib/Perlito/Runtime.pm         > libpy/Perlito__Runtime.py

perl perlito6.pl -Cpython src6/lib/Perlito/Javascript/Emitter.pm > libpy/Perlito__Javascript__Emitter.py
perl perlito6.pl -Cpython src6/lib/Perlito/Lisp/Emitter.pm    > libpy/Perlito__Lisp__Emitter.py
perl perlito6.pl -Cpython src6/lib/Perlito/Perl5/Emitter.pm   > libpy/Perlito__Perl5__Emitter.py
perl perlito6.pl -Cpython src6/lib/Perlito/Go/Emitter.pm      > libpy/Perlito__Go__Emitter.py
perl perlito6.pl -Cpython src6/lib/Perlito/Parrot/Emitter.pm  > libpy/Perlito__Parrot__Emitter.py
perl perlito6.pl -Cpython src6/lib/Perlito/Python/Emitter.pm  > libpy/Perlito__Python__Emitter.py
perl perlito6.pl -Cpython src6/lib/Perlito/Ruby/Emitter.pm    > libpy/Perlito__Ruby__Emitter.py
perl perlito6.pl -Cpython src6/lib/Perlito/Java/Emitter.pm    > libpy/Perlito__Java__Emitter.py

perl perlito6.pl -Cpython src6/util/perlito6.pl                    > ./perlito6.py



export PERL5LIB=lib5

rm -rf libpy/

mkdir libpy
touch libpy/__init__.py

cp src/lib/Perlito/Python/Runtime.py libpy/Perlito__Python__Runtime.py

perl perlito.pl -Cpython src/lib/Perlito/Python/Prelude.pm  > libpy/Perlito__Python__Prelude.py
perl perlito.pl -Cpython src/lib/Perlito/Precedence.pm      > libpy/Perlito__Precedence.py
perl perlito.pl -Cpython src/lib/Perlito/Expression.pm      > libpy/Perlito__Expression.py
perl perlito.pl -Cpython src/lib/Perlito/Macro.pm           > libpy/Perlito__Macro.py
perl perlito.pl -Cpython src/lib/Perlito/Test.pm            > libpy/Perlito__Test.py
perl perlito.pl -Cpython src/lib/Perlito/Grammar.pm         > libpy/Perlito__Grammar.py
perl perlito.pl -Cpython src/lib/Perlito/Grammar/Control.pm > libpy/Perlito__Grammar__Control.py
perl perlito.pl -Cpython src/lib/Perlito/Grammar/Regex.pm   > libpy/Perlito__Grammar__Regex.py
perl perlito.pl -Cpython src/lib/Perlito/Emitter/Token.pm   > libpy/Perlito__Emitter__Token.py
perl perlito.pl -Cpython src/lib/Perlito/Eval.pm            > libpy/Perlito__Eval.py
perl perlito.pl -Cpython src/lib/Perlito/Runtime.pm         > libpy/Perlito__Runtime.py

perl perlito.pl -Cpython src/lib/Perlito/Javascript/Emitter.pm > libpy/Perlito__Javascript__Emitter.py
perl perlito.pl -Cpython src/lib/Perlito/Lisp/Emitter.pm    > libpy/Perlito__Lisp__Emitter.py
perl perlito.pl -Cpython src/lib/Perlito/Perl5/Emitter.pm   > libpy/Perlito__Perl5__Emitter.py
perl perlito.pl -Cpython src/lib/Perlito/Go/Emitter.pm      > libpy/Perlito__Go__Emitter.py
perl perlito.pl -Cpython src/lib/Perlito/Parrot/Emitter.pm  > libpy/Perlito__Parrot__Emitter.py
perl perlito.pl -Cpython src/lib/Perlito/Python/Emitter.pm  > libpy/Perlito__Python__Emitter.py
perl perlito.pl -Cpython src/lib/Perlito/Ruby/Emitter.pm    > libpy/Perlito__Ruby__Emitter.py
perl perlito.pl -Cpython src/lib/Perlito/Java/Emitter.pm    > libpy/Perlito__Java__Emitter.py

perl perlito.pl -Cpython src/util/perlito.pl                    > ./perlito.py


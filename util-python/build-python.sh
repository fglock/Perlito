
export PERL5LIB=lib5

rm -rf libpy/

mkdir libpy
touch libpy/__init__.py

cp lib/Perlito/Python/Runtime.py libpy/Perlito__Python__Runtime.py

perl perlito.pl -Cpython lib/Perlito/Python/Prelude.pm  > libpy/Perlito__Python__Prelude.py
perl perlito.pl -Cpython lib/Perlito/Precedence.pm      > libpy/Perlito__Precedence.py
perl perlito.pl -Cpython lib/Perlito/Expression.pm      > libpy/Perlito__Expression.py
perl perlito.pl -Cpython lib/Perlito/Macro.pm           > libpy/Perlito__Macro.py
perl perlito.pl -Cpython lib/Perlito/Test.pm            > libpy/Perlito__Test.py
perl perlito.pl -Cpython lib/Perlito/Grammar.pm         > libpy/Perlito__Grammar.py
perl perlito.pl -Cpython lib/Perlito/Grammar/Control.pm > libpy/Perlito__Grammar__Control.py
perl perlito.pl -Cpython lib/Perlito/Grammar/Regex.pm   > libpy/Perlito__Grammar__Regex.py
perl perlito.pl -Cpython lib/Perlito/Emitter/Token.pm   > libpy/Perlito__Emitter__Token.py
perl perlito.pl -Cpython lib/Perlito/Eval.pm            > libpy/Perlito__Eval.py

perl perlito.pl -Cpython lib/Perlito/Javascript/Emitter.pm > libpy/Perlito__Javascript__Emitter.py
perl perlito.pl -Cpython lib/Perlito/Lisp/Emitter.pm    > libpy/Perlito__Lisp__Emitter.py
perl perlito.pl -Cpython lib/Perlito/Perl5/Emitter.pm   > libpy/Perlito__Perl5__Emitter.py
perl perlito.pl -Cpython lib/Perlito/Go/Emitter.pm      > libpy/Perlito__Go__Emitter.py
perl perlito.pl -Cpython lib/Perlito/Parrot/Emitter.pm  > libpy/Perlito__Parrot__Emitter.py
perl perlito.pl -Cpython lib/Perlito/Python/Emitter.pm  > libpy/Perlito__Python__Emitter.py
perl perlito.pl -Cpython lib/Perlito/Ruby/Emitter.pm    > libpy/Perlito__Ruby__Emitter.py

perl perlito.pl -Cpython util/perlito.pl                    > ./perlito.py


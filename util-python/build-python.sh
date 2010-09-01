
rm -rf libpy/

mkdir libpy
touch libpy/__init__.py

mkdir libpy/miniperl6
touch libpy/miniperl6/__init__.py

mkdir libpy/miniperl6/python
touch libpy/miniperl6/python/__init__.py

cp lib/Perlito/Python/Runtime.py libpy/miniperl6/python/runtime.py

perl mp6.pl -Cpython lib/Perlito/Python/Prelude.pm > libpy/Perlito__Python__Prelude.py
perl mp6.pl -Cpython lib/Test.pm                    > libpy/Test.py
perl mp6.pl -Cpython lib/Perlito/Grammar.pm       > libpy/Perlito__Grammar.py
perl mp6.pl -Cpython lib/Perlito/Grammar/Control.pm > libpy/Perlito__Grammar__Control.py
perl mp6.pl -Cpython lib/Perlito/Grammar/Mapping.pm > libpy/Perlito__Grammar__Mapping.py
perl mp6.pl -Cpython lib/Perlito/Grammar/Regex.pm > libpy/Perlito__Grammar__Regex.py
perl mp6.pl -Cpython lib/Perlito/Emitter/Token.pm > libpy/Perlito__Emitter__Token.py
perl mp6.pl -Cpython lib/Perlito/Eval.pm          > libpy/Perlito__Eval.py

perl mp6.pl -Cpython lib/Perlito/Javascript/Emitter.pm > libpy/Perlito__Javascript__Emitter.py
perl mp6.pl -Cpython lib/Perlito/Lisp/Emitter.pm  > libpy/Perlito__Lisp__Emitter.py
perl mp6.pl -Cpython lib/Perlito/Perl5/Emitter.pm > libpy/Perlito__Perl5__Emitter.py
perl mp6.pl -Cpython lib/Perlito/Go/Emitter.pm    > libpy/Perlito__Go__Emitter.py
perl mp6.pl -Cpython lib/Perlito/Parrot/Emitter.pm > libpy/Perlito__Parrot__Emitter.py
perl mp6.pl -Cpython lib/Perlito/Python/Emitter.pm > libpy/Perlito__Python__Emitter.py
perl mp6.pl -Cpython lib/Perlito/Ruby/Emitter.pm   > libpy/Perlito__Ruby__Emitter.py

perl mp6.pl -Cpython util/mp6.pl                    > ./mp6.py


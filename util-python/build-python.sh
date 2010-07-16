
rm -rf libpy/

mkdir libpy
touch libpy/__init__.py

mkdir libpy/miniperl6
touch libpy/miniperl6/__init__.py

mkdir libpy/miniperl6/python
touch libpy/miniperl6/python/__init__.py

cp lib/MiniPerl6/Python/Runtime.py libpy/miniperl6/python/runtime.py

perl mp6.pl -Cpython lib/MiniPerl6/Python/Prelude.pm > libpy/MiniPerl6__Python__Prelude.py
perl mp6.pl -Cpython lib/Test.pm                    > libpy/Test.py
perl mp6.pl -Cpython lib/MiniPerl6/Grammar.pm       > libpy/MiniPerl6__Grammar.py
perl mp6.pl -Cpython lib/MiniPerl6/Grammar/Control.pm > libpy/MiniPerl6__Grammar__Control.py
perl mp6.pl -Cpython lib/MiniPerl6/Grammar/Mapping.pm > libpy/MiniPerl6__Grammar__Mapping.py
perl mp6.pl -Cpython lib/MiniPerl6/Grammar/Regex.pm > libpy/MiniPerl6__Grammar__Regex.py
perl mp6.pl -Cpython lib/MiniPerl6/Emitter/Token.pm > libpy/MiniPerl6__Emitter__Token.py
perl mp6.pl -Cpython lib/MiniPerl6/Eval.pm          > libpy/MiniPerl6__Eval.py

perl mp6.pl -Cpython lib/MiniPerl6/Javascript/Emitter.pm > libpy/MiniPerl6__Javascript__Emitter.py
perl mp6.pl -Cpython lib/MiniPerl6/Lisp/Emitter.pm  > libpy/MiniPerl6__Lisp__Emitter.py
perl mp6.pl -Cpython lib/MiniPerl6/Perl5/Emitter.pm > libpy/MiniPerl6__Perl5__Emitter.py
perl mp6.pl -Cpython lib/MiniPerl6/Go/Emitter.pm    > libpy/MiniPerl6__Go__Emitter.py
perl mp6.pl -Cpython lib/MiniPerl6/Parrot/Emitter.pm > libpy/MiniPerl6__Parrot__Emitter.py

perl mp6.pl -Cpython util/mp6.pl                    > ./mp6.py


# Compile MiniPerl6 to Javascript, using mp6-javascript.pl

#rm -rf libjs
mkdir libjs
mkdir libjs/MiniPerl6
mkdir libjs/MiniPerl6/Grammar
mkdir libjs/MiniPerl6/Javascript
mkdir libjs/MiniPerl6/Emitter
mkdir libjs/MiniPerl6/Perl5
mkdir libjs/MiniPerl6/Lisp
# mkdir libjs/MiniPerl6/Perl5MO
# mkdir libjs/MiniPerl6/Parrot
# mkdir libjs/MiniPerl6/PAST
# mkdir libjs/MiniPerl6/Perl6Parrot

cp lib/MiniPerl6/Javascript/Runtime.js libjs/MiniPerl6/Javascript/

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Javascript/Prelude.pm    \
    >  libjs/MiniPerl6/Javascript/Prelude.js

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Javascript/Emitter.pm    \
    >  libjs/MiniPerl6/Javascript/Emitter.js

perl mp6-javascript.pl \
    <    lib/Test.pm    \
    >  libjs/Test.js

# perl mp6-javascript.pl \
#    <    lib/MiniPerl6/AST/CompUnit.pm    \
#    >  libjs/MiniPerl6/AST/CompUnit.js

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Grammar.pm    \
    >  libjs/MiniPerl6/Grammar.js

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Grammar/Control.pm    \
    >  libjs/MiniPerl6/Grammar/Control.js

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Grammar/Mapping.pm    \
    >  libjs/MiniPerl6/Grammar/Mapping.js

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Grammar/Regex.pm    \
    >  libjs/MiniPerl6/Grammar/Regex.js

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Emitter/Token.pm    \
    >  libjs/MiniPerl6/Emitter/Token.js

perl mp6-javascript.pl \
    <    lib/MiniPerl6/Perl5/Emitter.pm    \
    >  libjs/MiniPerl6/Perl5/Emitter.js


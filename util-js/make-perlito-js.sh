
. util-perl5/update-ast-perl5.sh

perl util-perl5/make.pl -Cjs

rm -rf libjs 
mv libjs-new libjs

cat libjs/MiniPerl6/Javascript/Runtime.js \
    libjs/MiniPerl6/Javascript/Prelude.js \
    libjs/MiniPerl6/Javascript/Emitter.js \
    libjs/MiniPerl6/Grammar.js         \
    libjs/MiniPerl6/Grammar/Control.js \
    libjs/MiniPerl6/Grammar/Regex.js   \
    libjs/MiniPerl6/Emitter/Token.js   \
    libjs/MiniPerl6/Precedence.js   \
    libjs/MiniPerl6/Expression.js   \
  > perlito.js

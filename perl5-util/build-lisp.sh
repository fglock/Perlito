# Compile MiniPerl6 to lisp, using mp6-perl5.pl

#rm -rf liblisp
mkdir liblisp
mkdir liblisp/MiniPerl6
mkdir liblisp/MiniPerl6/Grammar
mkdir liblisp/MiniPerl6/Lisp
mkdir liblisp/MiniPerl6/Emitter
mkdir liblisp/MiniPerl6/Perl5
mkdir liblisp/MiniPerl6/Javascript
# mkdir liblisp/MiniPerl6/Perl5MO
# mkdir liblisp/MiniPerl6/Parrot
# mkdir liblisp/MiniPerl6/PAST
# mkdir liblisp/MiniPerl6/Perl6Parrot

cp lib/MiniPerl6/Lisp/Runtime.lisp liblisp/MiniPerl6/Lisp/

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Lisp/Prelude.pm    \
    >  liblisp/MiniPerl6/Lisp/Prelude.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Lisp/Emitter.pm    \
    >  liblisp/MiniPerl6/Lisp/Emitter.lisp

perl mp6-lisp.pl \
    <      lib/Test.pm    \
    >  liblisp/Test.lisp

# perl mp6-lisp.pl \
#    <      lib/MiniPerl6/AST/CompUnit.pm    \
#    >  liblisp/MiniPerl6/AST/CompUnit.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Grammar.pm    \
    >  liblisp/MiniPerl6/Grammar.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Grammar/Control.pm    \
    >  liblisp/MiniPerl6/Grammar/Control.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Grammar/Mapping.pm    \
    >  liblisp/MiniPerl6/Grammar/Mapping.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Grammar/Regex.pm    \
    >  liblisp/MiniPerl6/Grammar/Regex.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Emitter/Token.pm    \
    >  liblisp/MiniPerl6/Emitter/Token.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Perl5/Emitter.pm    \
    >  liblisp/MiniPerl6/Perl5/Emitter.lisp


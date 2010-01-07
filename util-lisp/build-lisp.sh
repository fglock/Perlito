# Compile MiniPerl6 to lisp, using mp6-lisp.lisp

rm -rf liblisp-new
mkdir liblisp-new
mkdir liblisp-new/MiniPerl6
mkdir liblisp-new/MiniPerl6/Grammar
mkdir liblisp-new/MiniPerl6/Lisp
mkdir liblisp-new/MiniPerl6/Go
mkdir liblisp-new/MiniPerl6/Emitter
mkdir liblisp-new/MiniPerl6/Perl5
mkdir liblisp-new/MiniPerl6/Javascript

cp lib/MiniPerl6/Lisp/Runtime.lisp liblisp-new/MiniPerl6/Lisp/

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Lisp/Prelude.pm    \
    >  liblisp-new/MiniPerl6/Lisp/Prelude.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Lisp/Emitter.pm    \
    >  liblisp-new/MiniPerl6/Lisp/Emitter.lisp

sbcl --script mp6-lisp.lisp \
               lib/Test.pm    \
    >  liblisp-new/Test.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Grammar.pm    \
    >  liblisp-new/MiniPerl6/Grammar.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Grammar/Control.pm    \
    >  liblisp-new/MiniPerl6/Grammar/Control.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Grammar/Mapping.pm    \
    >  liblisp-new/MiniPerl6/Grammar/Mapping.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Grammar/Regex.pm    \
    >  liblisp-new/MiniPerl6/Grammar/Regex.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Emitter/Token.pm    \
    >  liblisp-new/MiniPerl6/Emitter/Token.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Perl5/Emitter.pm    \
    >  liblisp-new/MiniPerl6/Perl5/Emitter.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Javascript/Emitter.pm    \
    >  liblisp-new/MiniPerl6/Javascript/Emitter.lisp

sbcl --script mp6-lisp.lisp \
               lib/MiniPerl6/Go/Emitter.pm    \
    >  liblisp-new/MiniPerl6/Go/Emitter.lisp


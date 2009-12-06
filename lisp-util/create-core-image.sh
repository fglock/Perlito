cat liblisp/MiniPerl6/Lisp/Runtime.lisp \
    liblisp/MiniPerl6/Lisp/Prelude.lisp \
    liblisp/MiniPerl6/Lisp/Emitter.lisp \
    liblisp/MiniPerl6/Grammar.lisp \
    liblisp/MiniPerl6/Grammar/Control.lisp \
    liblisp/MiniPerl6/Grammar/Mapping.lisp \
    liblisp/MiniPerl6/Grammar/Regex.lisp   \
    liblisp/MiniPerl6/Emitter/Token.lisp   \
    lisp-util/create-core-image.lisp   \
  > mp6-lisp-create-core-image.lisp

sbcl --script  mp6-lisp-create-core-image.lisp 


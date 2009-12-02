rm -rf liblisp-new
rm -rf liblisp-old
rm -rf liblisp-tmp
echo 'Compiling with old version'
. lisp-util/build-lisp.sh
mv liblisp liblisp-old
mv liblisp-new liblisp
echo 'Compiling with new version'
. lisp-util/build-lisp.sh
mv liblisp liblisp-tmp
mv liblisp-new liblisp
echo 'Finished. Old version moved to liblisp-old. " diff -r liblisp liblisp-tmp "  should show no differences'


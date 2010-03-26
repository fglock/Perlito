rm -rf libast-perl5
perl util-perl5/make.pl -Cast-perl5 && rm -rf libast-perl5 && mv libast-perl5-new libast-perl5
perl util-perl5/make.pl -Cperl5 && rm -rf lib5 && mv lib5-new lib5
perl util-perl5/make.pl -Clisp && rm -rf liblisp && mv liblisp-new liblisp
perl util-perl5/make.pl -Cjs && rm -rf libjs && mv libjs-new libjs
. util-js/make-perlito-js.sh
. util-lisp/prepare-mp6-lisp-main.sh
. util-lisp/create-core-image.sh

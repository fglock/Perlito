rm -rf libast-perl5
perl util-perl5/make.pl -Cast-perl5 && rm -rf libast-perl5 && mv libast-perl5-new libast-perl5
perl util-perl5/make.pl -Cperl5 && rm -rf lib5 && mv lib5-new lib5
perl util-perl5/make.pl -Cjs && rm -rf libjs && mv libjs-new libjs
perl mp6.pl -Clisp-bin util/mp6.pl && mv tmp.out mp6-lisp
. util-js/make-perlito-js.sh
. util-python/build-python.sh 
. util-ruby/build-ruby.sh 


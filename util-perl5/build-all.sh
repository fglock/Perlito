rm -rf libast-perl5

perl util-perl5/make.pl -Cast-perl5 && rm -rf libast-perl5 && mv libast-perl5-new libast-perl5

perl util-perl5/make.pl -Cperl5 && rm -rf lib5 && mv lib5-new lib5

perl util-perl5/make.pl -Cjs && rm -rf libjs && mv libjs-new libjs

rm tmp.out
perl perlito.pl -Clisp-bin util/perlito.pl && mv tmp.out perlito-lisp

rm 6.out
perl perlito.pl -Cgo-bin util/perlito.pl && mv 6.out perlito-go

. util-js/make-perlito-js.sh

. util-python/build-python.sh 

. util-ruby/build-ruby.sh 


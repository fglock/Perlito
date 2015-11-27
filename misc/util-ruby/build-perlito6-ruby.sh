
export PERL5LIB=lib5

rm -rf librb/

mkdir librb
mkdir librb/Perlito6
mkdir librb/Perlito6/Ruby

cp src6/lib/Perlito6/Ruby/Runtime.rb librb/Perlito6/Ruby/Runtime.rb

perl perlito6.pl -Cruby src6/lib/Perlito6/Test.pm            > librb/Perlito6/Test.rb

perl perlito6.pl -Cruby src6/lib/Perlito6/AST.pm             > librb/Perlito6__AST.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Grammar.pm         > librb/Perlito6__Grammar.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Grammar/Control.pm > librb/Perlito6__Grammar__Control.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Grammar/Regex.pm   > librb/Perlito6__Grammar__Regex.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Emitter/Token.pm   > librb/Perlito6__Emitter__Token.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Eval.pm            > librb/Perlito6__Eval.rb

perl perlito6.pl -Cruby src6/lib/Perlito6/Javascript/Emitter.pm > librb/Perlito6__Javascript__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Lisp/Emitter.pm    > librb/Perlito6__Lisp__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Perl5/Emitter.pm   > librb/Perlito6__Perl5__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Go/Emitter.pm      > librb/Perlito6__Go__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Parrot/Emitter.pm  > librb/Perlito6__Parrot__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Python/Emitter.pm  > librb/Perlito6__Python__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Ruby/Emitter.pm    > librb/Perlito6__Ruby__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito6/Ruby/Prelude.pm    > librb/Perlito6__Ruby__Prelude.rb

perl perlito6.pl -Cruby src6/util/perlito6.pl                      > ./perlito6.rb


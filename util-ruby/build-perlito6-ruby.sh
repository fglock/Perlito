
export PERL5LIB=lib5

rm -rf librb/

mkdir librb
mkdir librb/Perlito
mkdir librb/Perlito/Ruby

cp src6/lib/Perlito/Ruby/Runtime.rb librb/Perlito/Ruby/Runtime.rb

perl perlito6.pl -Cruby src6/lib/Perlito/Test.pm            > librb/Perlito/Test.rb

perl perlito6.pl -Cruby src6/lib/Perlito/AST.pm             > librb/Perlito__AST.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Grammar.pm         > librb/Perlito__Grammar.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Grammar/Control.pm > librb/Perlito__Grammar__Control.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Grammar/Regex.pm   > librb/Perlito__Grammar__Regex.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Emitter/Token.pm   > librb/Perlito__Emitter__Token.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Eval.pm            > librb/Perlito__Eval.rb

perl perlito6.pl -Cruby src6/lib/Perlito/Javascript/Emitter.pm > librb/Perlito__Javascript__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Lisp/Emitter.pm    > librb/Perlito__Lisp__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Perl5/Emitter.pm   > librb/Perlito__Perl5__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Go/Emitter.pm      > librb/Perlito__Go__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Parrot/Emitter.pm  > librb/Perlito__Parrot__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Python/Emitter.pm  > librb/Perlito__Python__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Ruby/Emitter.pm    > librb/Perlito__Ruby__Emitter.rb
perl perlito6.pl -Cruby src6/lib/Perlito/Ruby/Prelude.pm    > librb/Perlito__Ruby__Prelude.rb

perl perlito6.pl -Cruby src6/util/perlito6.pl                      > ./perlito6.rb


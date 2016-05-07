
export RUBYLIB=librb

rm -rf librb-new/

mkdir librb-new
mkdir librb-new/Perlito6
mkdir librb-new/Perlito6/Ruby

cp src6/lib/Perlito6/Ruby/Runtime.rb librb-new/Perlito6/Ruby/Runtime.rb

ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Test.pm            > librb-new/Perlito6__Test.rb

ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/AST.pm             > librb-new/Perlito6__AST.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Grammar.pm         > librb-new/Perlito6__Grammar.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Grammar/Control.pm > librb-new/Perlito6__Grammar__Control.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Grammar/Regex.pm   > librb-new/Perlito6__Grammar__Regex.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Emitter/Token.pm   > librb-new/Perlito6__Emitter__Token.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Eval.pm            > librb-new/Perlito6__Eval.rb

ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/JavaScript/Emitter.pm > librb-new/Perlito6__JavaScript__Emitter.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Lisp/Emitter.pm    > librb-new/Perlito6__Lisp__Emitter.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Perl5/Emitter.pm   > librb-new/Perlito6__Perl5__Emitter.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Go/Emitter.pm      > librb-new/Perlito6__Go__Emitter.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Parrot/Emitter.pm  > librb-new/Perlito6__Parrot__Emitter.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Python/Emitter.pm  > librb-new/Perlito6__Python__Emitter.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Ruby/Emitter.pm    > librb-new/Perlito6__Ruby__Emitter.rb
ruby1.9 perlito6.rb -Cruby src6/lib/Perlito6/Ruby/Prelude.pm    > librb-new/Perlito6__Ruby__Prelude.rb

ruby1.9 perlito6.rb -Cruby src6/util/perlito6.pl                      > ./perlito-new.rb

rm -rf librb-old/
mv librb librb-old
mv librb-new librb

rm perlito-old.rb
mv perlito6.rb perlito-old.rb
mv perlito-new.rb perlito6.rb



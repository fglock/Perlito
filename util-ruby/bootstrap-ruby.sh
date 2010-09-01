
export RUBYLIB=librb

rm -rf librb-new/

mkdir librb-new
mkdir librb-new/Perlito
mkdir librb-new/Perlito/Ruby

cp lib/Perlito/Ruby/Runtime.rb librb-new/Perlito/Ruby/Runtime.rb

ruby1.9 perlito.rb -Cruby lib/Perlito/Test.pm            > librb-new/Perlito__Test.rb

ruby1.9 perlito.rb -Cruby lib/Perlito/Grammar.pm         > librb-new/Perlito__Grammar.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Grammar/Control.pm > librb-new/Perlito__Grammar__Control.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Grammar/Mapping.pm > librb-new/Perlito__Grammar__Mapping.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Grammar/Regex.pm   > librb-new/Perlito__Grammar__Regex.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Emitter/Token.pm   > librb-new/Perlito__Emitter__Token.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Eval.pm            > librb-new/Perlito__Eval.rb

ruby1.9 perlito.rb -Cruby lib/Perlito/Javascript/Emitter.pm > librb-new/Perlito__Javascript__Emitter.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Lisp/Emitter.pm    > librb-new/Perlito__Lisp__Emitter.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Perl5/Emitter.pm   > librb-new/Perlito__Perl5__Emitter.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Go/Emitter.pm      > librb-new/Perlito__Go__Emitter.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Parrot/Emitter.pm  > librb-new/Perlito__Parrot__Emitter.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Python/Emitter.pm  > librb-new/Perlito__Python__Emitter.rb
ruby1.9 perlito.rb -Cruby lib/Perlito/Ruby/Emitter.pm    > librb-new/Perlito__Ruby__Emitter.rb

ruby1.9 perlito.rb -Cruby util/perlito.pl                      > ./perlito-new.rb

rm -rf librb-old/
mv librb librb-old
mv librb-new librb

rm perlito-old.rb
mv perlito.rb perlito-old.rb
mv perlito-new.rb perlito.rb



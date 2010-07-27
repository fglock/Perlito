
export RUBYLIB=librb

rm -rf librb-new/

mkdir librb-new
mkdir librb-new/MiniPerl6
mkdir librb-new/MiniPerl6/Ruby

cp lib/MiniPerl6/Ruby/Runtime.rb librb-new/MiniPerl6/Ruby/Runtime.rb

ruby1.9 mp6.rb -Cruby lib/Test.pm                      > librb-new/Test.rb

ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Grammar.pm         > librb-new/MiniPerl6__Grammar.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Grammar/Control.pm > librb-new/MiniPerl6__Grammar__Control.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Grammar/Mapping.pm > librb-new/MiniPerl6__Grammar__Mapping.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Grammar/Regex.pm   > librb-new/MiniPerl6__Grammar__Regex.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Emitter/Token.pm   > librb-new/MiniPerl6__Emitter__Token.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Eval.pm            > librb-new/MiniPerl6__Eval.rb

ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Javascript/Emitter.pm > librb-new/MiniPerl6__Javascript__Emitter.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Lisp/Emitter.pm    > librb-new/MiniPerl6__Lisp__Emitter.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Perl5/Emitter.pm   > librb-new/MiniPerl6__Perl5__Emitter.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Go/Emitter.pm      > librb-new/MiniPerl6__Go__Emitter.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Parrot/Emitter.pm  > librb-new/MiniPerl6__Parrot__Emitter.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Python/Emitter.pm  > librb-new/MiniPerl6__Python__Emitter.rb
ruby1.9 mp6.rb -Cruby lib/MiniPerl6/Ruby/Emitter.pm    > librb-new/MiniPerl6__Ruby__Emitter.rb

ruby1.9 mp6.rb -Cruby util/mp6.pl                      > ./mp6-new.rb

rm -rf librb-old/
mv librb librb-old
mv librb-new librb

rm mp6-old.rb
mv mp6.rb mp6-old.rb
mv mp6-new.rb mp6.rb



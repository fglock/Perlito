
rm -rf librb/

mkdir librb
mkdir librb/Miniperl6
mkdir librb/Miniperl6/Ruby

cp lib/MiniPerl6/Ruby/Runtime.rb librb/Miniperl6/Ruby/Runtime.rb

perl mp6.pl -Cruby lib/Test.pm                      > librb/Test.rb

perl mp6.pl -Cruby lib/MiniPerl6/Grammar.pm         > librb/MiniPerl6__Grammar.rb
perl mp6.pl -Cruby lib/MiniPerl6/Grammar/Control.pm > librb/MiniPerl6__Grammar__Control.rb
perl mp6.pl -Cruby lib/MiniPerl6/Grammar/Mapping.pm > librb/MiniPerl6__Grammar__Mapping.rb
perl mp6.pl -Cruby lib/MiniPerl6/Grammar/Regex.pm   > librb/MiniPerl6__Grammar__Regex.rb
perl mp6.pl -Cruby lib/MiniPerl6/Emitter/Token.pm   > librb/MiniPerl6__Emitter__Token.rb
perl mp6.pl -Cruby lib/MiniPerl6/Eval.pm            > librb/MiniPerl6__Eval.rb

perl mp6.pl -Cruby lib/MiniPerl6/Javascript/Emitter.pm > librb/MiniPerl6__Javascript__Emitter.rb
perl mp6.pl -Cruby lib/MiniPerl6/Lisp/Emitter.pm    > librb/MiniPerl6__Lisp__Emitter.rb
perl mp6.pl -Cruby lib/MiniPerl6/Perl5/Emitter.pm   > librb/MiniPerl6__Perl5__Emitter.rb
perl mp6.pl -Cruby lib/MiniPerl6/Go/Emitter.pm      > librb/MiniPerl6__Go__Emitter.rb
perl mp6.pl -Cruby lib/MiniPerl6/Parrot/Emitter.pm  > librb/MiniPerl6__Parrot__Emitter.rb
perl mp6.pl -Cruby lib/MiniPerl6/Python/Emitter.pm  > librb/MiniPerl6__Python__Emitter.rb
perl mp6.pl -Cruby lib/MiniPerl6/Ruby/Emitter.pm    > librb/MiniPerl6__Ruby__Emitter.rb

perl mp6.pl -Cruby util/mp6.pl                      > ./mp6.rb



export PERL5LIB=lib5

rm -rf librb/

mkdir librb
mkdir librb/Perlito
mkdir librb/Perlito/Ruby

cp lib/Perlito/Ruby/Runtime.rb librb/Perlito/Ruby/Runtime.rb

perl perlito.pl -Cruby lib/Perlito/Test.pm            > librb/Perlito/Test.rb

perl perlito.pl -Cruby lib/Perlito/Grammar.pm         > librb/Perlito__Grammar.rb
perl perlito.pl -Cruby lib/Perlito/Grammar/Control.pm > librb/Perlito__Grammar__Control.rb
perl perlito.pl -Cruby lib/Perlito/Grammar/Regex.pm   > librb/Perlito__Grammar__Regex.rb
perl perlito.pl -Cruby lib/Perlito/Emitter/Token.pm   > librb/Perlito__Emitter__Token.rb
perl perlito.pl -Cruby lib/Perlito/Eval.pm            > librb/Perlito__Eval.rb

perl perlito.pl -Cruby lib/Perlito/Javascript/Emitter.pm > librb/Perlito__Javascript__Emitter.rb
perl perlito.pl -Cruby lib/Perlito/Lisp/Emitter.pm    > librb/Perlito__Lisp__Emitter.rb
perl perlito.pl -Cruby lib/Perlito/Perl5/Emitter.pm   > librb/Perlito__Perl5__Emitter.rb
perl perlito.pl -Cruby lib/Perlito/Go/Emitter.pm      > librb/Perlito__Go__Emitter.rb
perl perlito.pl -Cruby lib/Perlito/Parrot/Emitter.pm  > librb/Perlito__Parrot__Emitter.rb
perl perlito.pl -Cruby lib/Perlito/Python/Emitter.pm  > librb/Perlito__Python__Emitter.rb
perl perlito.pl -Cruby lib/Perlito/Ruby/Emitter.pm    > librb/Perlito__Ruby__Emitter.rb

perl perlito.pl -Cruby util/perlito.pl                      > ./perlito.rb


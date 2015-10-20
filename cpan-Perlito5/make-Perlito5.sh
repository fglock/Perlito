[ -d cpan-Perlito5 ] && cd cpan-Perlito5

rm -rf lib
rm -rf src
rm -rf t
rm -rf scripts

touch META.yml

cp ../ChangeLog ./Changes

mkdir t
cp -r ../t5/* t/

mkdir lib
cp -r ../src5/lib/* lib/

mkdir src
cp -r ../src5/lib/* src/

mkdir scripts
cp ../src5/util/perlito5.pl scripts/perlito5

perldoc -otext scripts/perlito5 > README

rm t/01-perlito/050-string.t             
rm t/01-perlito/190-bind-sub-param.t     
rm t/01-perlito/280-hash-autovivify.t    
rm t/01-perlito/350-syntax-namespace.t   
rm t/01-perlito/410-ampersand.t          
rm t/01-perlito/420-vstring.t            

# perldoc -opod scripts/perlito5 > lib/Perlito5.pm
# echo '1;' >> lib/Perlito5.pm

# ack -l '^s*token ' ../src5
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Bareword.pm    > lib/Perlito5/Grammar/Bareword.pm  
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Map.pm         > lib/Perlito5/Grammar/Map.pm       
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Print.pm       > lib/Perlito5/Grammar/Print.pm     
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Statement.pm   > lib/Perlito5/Grammar/Statement.pm     
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar.pm             > lib/Perlito5/Grammar.pm
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Number.pm      > lib/Perlito5/Grammar/Number.pm    
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Regex5.pm      > lib/Perlito5/Grammar/Regex5.pm    
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Use.pm         > lib/Perlito5/Grammar/Use.pm       
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Control.pm     > lib/Perlito5/Grammar/Control.pm   
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Expression.pm  > lib/Perlito5/Grammar/Expression.pm
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Space.pm       > lib/Perlito5/Grammar/Space.pm     
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Regex6.pm      > lib/Perlito5/Grammar/Regex6.pm    
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/Block.pm       > lib/Perlito5/Grammar/Block.pm     
perl ../perlito5.pl --bootstrapping -I ../src5/lib -Cperl5 ../src5/lib/Perlito5/Grammar/String.pm      > lib/Perlito5/Grammar/String.pm    

perl Makefile.PL


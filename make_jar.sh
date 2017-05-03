
make clean
perl perlito5.pl --bootstrapping -Isrc5/lib -Cjava src5/util/perlito5.pl > perlito5.java
time javac -J-Xms2000m -J-Xmx2000m -J-Xss2000m -source 7 perlito5.java
rm perlito5.jar
mkdir org
mkdir org/perlito
mkdir org/perlito/Perlito5
mv *.class org/perlito/Perlito5/
jar -cfe perlito5.jar org.perlito.Perlito5.Main org/perlito/Perlito5/*.class
rm -rf org



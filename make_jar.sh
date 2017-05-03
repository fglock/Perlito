
make clean
rm perlito5.jar
rm -rf org

perl perlito5.pl --bootstrapping -Isrc5/lib -Cjava src5/util/perlito5-javalib.pl > perlito5-javalib.java

mkdir org
mkdir org/perlito
mkdir org/perlito/Perlito5

perl make_jar_.pl perlito5-javalib.java

time javac -J-Xms2000m -J-Xmx2000m -J-Xss2000m -source 7 org/perlito/Perlito5/Main.java 

jar -cfe perlito5.jar org.perlito.Perlito5.Main org/perlito/Perlito5/*.class
# rm -rf org



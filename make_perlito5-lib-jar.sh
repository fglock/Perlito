
# cleanup

# make clean
rm perlito5-lib.jar
rm perlito5.jar
rm -rf org


# Step 1: compile and package the Perl lib

# Note: no --java_eval
perl perlito5.pl --bootstrapping --bootstrap_java_eval -Isrc5/lib -Cjava src5/util/perlito5-javalib.pl > perlito5-javalib.java

mkdir org
mkdir org/perlito
mkdir org/perlito/Perlito5

perl make_perlito5-lib-jar_.pl perlito5-javalib.java

time javac -J-Xms2000m -J-Xmx2000m -J-Xss2000m -source 7 org/perlito/Perlito5/LibPerl.java 
jar -cfe perlito5-lib.jar org.perlito.Perlito5.LibPerl org/perlito/Perlito5/*.class



# Step 2: compile and package the CLI

# Note: use --java_eval
perl perlito5.pl --bootstrapping --java_eval -Isrc5/lib -Cjava src5/util/jperl.pl > jperl.java

perl make_perlito5-lib-jar_.pl jperl.java 

# Note: compile with perlito5-lib.jar so eval-string works
# Note: jperl.pl contains class "Main"
javac -cp .:perlito5-lib.jar -source 7 org/perlito/Perlito5/Main.java

# repackage everything
jar -cfe perlito5.jar org.perlito.Perlito5.Main org/perlito/Perlito5/*.class



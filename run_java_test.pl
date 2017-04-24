# run_java_test.pl
#
# Use this script together with "prove" to run Perlito5-Java tests:
#
# prove -r -e 'perl run_java_test.pl' t5
#

my $t = shift;
print ` touch Main.class `;
print ` rm Main.class `;
print ` perl perlito5.pl -Isrc5/lib -I. -It -Cjava $t  > Main.java `;
print ` javac -source 7 Main.java `;
print ` java Main `;


__END__

Note: large source files seem to trigger this problem:

http://stackoverflow.com/questions/30707387/troubleshoot-slow-compilation
http://stackoverflow.com/questions/34223249/slow-compilation-with-jooq-3-6-plain-sql-and-the-javac-compiler

- "The workaround is to compile at Java 7-compatibility level: javac -source 7, or just to use simpler constructions.
- "the workaround is to introduce local variables when there are nested generic method calls that use generic type inference



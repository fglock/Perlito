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
print ` javac Main.java `;
print ` java Main `;


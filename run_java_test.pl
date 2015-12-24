my $t = shift;
print ` touch Main.class `;
print ` rm Main.class `;
print ` perl perlito5.pl -Isrc5/lib -I. -It -Cjava $t  > Main.java `;
print ` javac Main.java `;
print ` java Main `;


my $t = shift;
print ` touch Test.class `;
print ` rm Test.class `;
print ` perl perlito5.pl -Isrc5/lib -I. -It -Cjava $t  > Test.java `;
print ` javac Test.java `;
print ` java Test `;


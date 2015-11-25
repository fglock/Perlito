#!/usr/bin/perl
# begincheck
# extracted from "perlmod" documentation
#
print         "10. Ordinary code runs at runtime.\n";
END { print   "16.   So this is the end of the tale.\n" }
INIT { print  " 7. INIT blocks run FIFO just before runtime.\n" }
UNITCHECK {
  print       " 4.   And therefore before any CHECK blocks.\n"
}
CHECK { print " 6.   So this is the sixth line.\n" }
print         "11.   It runs in order, of course.\n";
BEGIN { print " 1. BEGIN blocks run FIFO during compilation.\n" }
END { print   "15.   Read perlmod for the rest of the story.\n" }
CHECK { print " 5. CHECK blocks run LIFO after all compilation.\n" }
INIT { print  " 8.   Run this again, using Perl's -c switch.\n" }
print         "12.   This is anti-obfuscated code.\n";
END { print   "14. END blocks run LIFO at quitting time.\n" }
BEGIN { print " 2.   So this line comes out second.\n" }
UNITCHECK {
 print " 3. UNITCHECK blocks run LIFO after each file is compiled.\n"
}
INIT { print  " 9.   You'll see the difference right away.\n" }
print         "13.   It only _looks_ like it should be confusing.\n";
__END__


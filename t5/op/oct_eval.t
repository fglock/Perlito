
print "1..2\n";
my $test = 1;

eval '$a = oct "10\x{100}"';
print $@ =~ /Wide character/ ? "ok $test\n" : "not ok $test\n"; $test++;

eval '$a = hex "ab\x{100}"';
print $@ =~ /Wide character/ ? "ok $test\n" : "not ok $test\n"; $test++;

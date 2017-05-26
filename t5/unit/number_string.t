use feature 'say';

say  "1..17";

print "not " if "1" != 1;
say  "ok 1 - Integer";
print "not " if "67000" != 67000;
say  "ok 2 - not small integer";
print "not " if "0" != 0;
say  "ok 3 - zero";

print "not " if "1.5" != 1.5;
say  "ok 4 - float";
print "not " if "-1" != -1;
say  "ok 5 - negative integer", -1;
print "not " if "-1.5" != -1.5;
say  "ok 6 - negative float";

# number + noise

print "not " if "1acmfd" != 1;
say  "ok 7 - Integer";
print "not " if "67000acmfd" != 67000;
say  "ok 8 - not small integer";
print "not " if "0acmfd" != 0;
say  "ok 9 - zero";

print "not " if "1.5acmfd" != 1.5;
say  "ok 10 - float";
print "not " if "-1acmfd" != -1;
say  "ok 11 - negative integer", -1;
print "not " if "-1.5acmfd" != -1.5;
say  "ok 12 - negative float";

print "not " if "033e4" != 330000;
say  "ok 13 - start with 0";

print "not " if "033e4-1652-4" != 330000;
say  "ok 14 - start with 0 + noise";

print "not " if "033e40549-1652-4" != "Inf";
say  "ok 15 - overflow to Inf";

print "not " if "547137e" != 547137;
say  "ok 16 - malformed exponent";
print "not " if "547137e-x" != 547137;
say  "ok 17 - malformed exponent";



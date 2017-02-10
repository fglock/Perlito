use feature 'say';

say  "1..12";

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


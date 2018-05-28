print "1..2\n";

*PI = \3.141592653; 

print "not " if $PI < 3.14 || $PI > 3.16;
print "ok 1 # $PI\n";

*PY = *PI;

print "not " if $PY < 3.14 || $PY > 3.16;
print "ok 2 # $PY\n";


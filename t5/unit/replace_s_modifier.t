
print "1..2\n";

$_ = "ar\nx";
s/r.//s;

print "not " if $_ ne 'ax';
print "ok 1  # $_ \n";

$_ = "ar\nx";
s/r.//;

print "not " if $_ ne "ar\nx";
print "ok 2  # without modifier \n";



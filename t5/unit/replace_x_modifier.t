
print "1..2\n";

$_ = "arnx";
s/r . # comment //x;

print "not " if $_ ne 'ax';
print "ok 1  # $_ \n";

$_ = "arnx";
s/r  # comment 
. //x;

print "not " if $_ ne 'ax';
print "ok 2  # $_ \n";



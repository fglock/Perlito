
print "1..1\n";

$_ = "arnx";
s/r . # comment //x;

print "not " if $_ ne 'ax';
print "ok 1  # $_ \n";




print "1..1\n";

$_ = 'ar';
s/(a)r/$1 . "x"/e;

print "not " if $_ ne 'ax';
print "ok 1  # $_ \n";



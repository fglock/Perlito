
print "1..1\n";

$_ = 'bar';
s/(a)r/x$1/;

print "not " if $_ ne 'bxa';
print "ok 1  # $_ \n";



print "1..3\n";

$_ = 'ar';
s/(a)r/x$1/;

print "not " if $_ ne 'xa';
print "ok 1  # $_ \n";

$_ = 'bar';
s/(a)r/x$1/;

print "not " if $_ ne 'bxa';
print "ok 2  # $_ \n";

$_ = 'barv';
s/(a)r/x$1/;

print "not " if $_ ne 'bxav';
print "ok 3  # $_ \n";


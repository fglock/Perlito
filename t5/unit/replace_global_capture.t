
print "1..3\n";

$_ = 'arbar';
s/(a)r/x$1/g;

print "not " if $_ ne 'xabxa';
print "ok 1  # $_ \n";

$_ = 'barvbar';
s/(a)r/x$1/;

print "not " if $_ ne 'bxavbar';
print "ok 2  # $_ \n";

$_ = 'barvbarv';
s/(a)r/x$1/;

print "not " if $_ ne 'bxavbarv';
print "ok 3  # $_ \n";


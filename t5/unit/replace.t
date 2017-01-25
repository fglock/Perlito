
print "1..2\n";

$_ = 'bar';
s/a/x/;

print "not " if $_ ne 'bxr';
print "ok 1  # $_ \n";

$_ = 'bar';
s/\w/x/;

print "not " if $_ ne 'xar';
print "ok 2  # $_ \n";


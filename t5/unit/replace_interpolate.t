
print "1..3\n";

my $reg = "(a)";

$_ = 'ar';
s/${reg}r/x$1/;

print "not " if $_ ne 'xa';
print "ok 1  # $_ \n";

$_ = 'bar';
s/${reg}r/x$1/;

print "not " if $_ ne 'bxa';
print "ok 2  # $_ \n";

$_ = 'barv';
s/${reg}r/x$1/;

print "not " if $_ ne 'bxav';
print "ok 3  # $_ \n";


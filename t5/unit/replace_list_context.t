
print "1..3\n";

my @r;

$_ = 'bar';
@r = s/a/x/;
print "not " if "@r" ne '1';
print "ok 1 - list context match # @r \n";

$_ = 'bar';
@r = s/w/x/;
print "not " if "@r" ne '';
print "ok 2 - list context don't match # @r \n";

$_ = 'bar';
@r = s/(a)(r)/x/;
print "not " if "@r" ne '1';
print "ok 3 - list context don't match # @r \n";


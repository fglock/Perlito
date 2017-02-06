
print "1..4\n";

$_ = 'bar';
s/a/x/;

print "not " if $_ ne 'bxr';
print "ok 1  # $_ \n";

$_ = 'bar';
s/\w/x/;

print "not " if $_ ne 'xar';
print "ok 2  # $_ \n";

# scalar context

$_ = 'bar';
my $r = s/a/x/;
print "not " if $r ne '1';
print "ok 3 - scalar match # $r \n";

my $r = s/w/x/;
print "not " if $r ne '';
print "ok 4 - scalar don't match # $r \n";


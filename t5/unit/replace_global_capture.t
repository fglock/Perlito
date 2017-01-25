
print "1..6\n";

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

# scalar context

$_ = 'barvar';
my $r = s/(a)(r)/x/g;
print "not " if $r ne '2';
print "ok 4 - scalar match # $r \n";

my $r = s/w/x/g;
print "not " if $r ne '';
print "ok 5 - scalar don't match # $r \n";

$_ = 'barvar';
my $r = s/a/x/g;
print "not " if $r ne '2';
print "ok 6 - scalar match no captures # $r \n";


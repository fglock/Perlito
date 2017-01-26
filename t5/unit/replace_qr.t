
print "1..3\n";

my $qr = qr/A/i;

$_ = 'barbAr';
s/$qr/x/;

print "not " if $_ ne 'bxrbAr';
print "ok 1  # $_ /$qr/ \n";

$qr = qr/A/;

$_ = 'barbAr';
s/$qr/x/;

print "not " if $_ ne 'barbxr';
print "ok 2  # $_ /$qr/ \n";

$_ = 'barbAr';
s/$qr/x/i;

print "not " if $_ ne 'barbxr';
print "ok 3 - inverting a flag has no effect # $_ /$qr/ \n";


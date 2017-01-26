
print "1..2\n";

my $qr = qr/A/i;

$_ = 'barbAr';
s/$qr/x/;

print "not " if $_ ne 'bxrbAr';
print "ok 1  # $_ /$qr/ \n";

my $qr = qr/A/;

$_ = 'barbAr';
s/$qr/x/;

print "not " if $_ ne 'barbxr';
print "ok 2  # $_ /$qr/ \n";



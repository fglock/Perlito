
print "1..1\n";

my $qr = qr/A/i;

$_ = 'bar';
s/$qr/x/;

print "not " if $_ ne 'bxr';
print "ok 1  # $_ \n";



print "1..1\n";

my $DATA = "aaa
bbb
ccc
";

my $pkg = 'main';

open *{"$pkg::DATA"}, '<', \$DATA;
my $v = readline( *{"$pkg::DATA"} );
chomp $v;

print "not " if $v ne "aaa";
print "ok 1 # $v\n";


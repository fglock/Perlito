
# create "binary" files with different encodings;
#
# test with:
# $ hexdump file.bin 
# $ hexdump utf8.bin 

unlink "file.bin";
open OUT, ">", "file.bin"
    or die $!;
for (0..255) {
    print OUT chr($_);
}
close OUT;

unlink "utf8.bin";
open OUT, "> :encoding(UTF-8)", "utf8.bin"
    or die $!;
for (0..255) {
    print OUT chr($_);
}
close OUT;

open IN, "<", "file.bin"
    or die $!;
my $i = 0;
while ( defined( my $c = getc(IN) ) ) {
    printf "\n%04x ", $i
        if ($i % 16) == 0;
    printf " %02x", ord($c);
    $i++;
}
print "\n";

open IN, "< :encoding(UTF-8)", "utf8.bin"
    or die $!;
my $i = 0;
while ( defined( my $c = getc(IN) ) ) {
    printf "\n%04x ", $i
        if ($i % 16) == 0;
    printf " %02x", ord($c);
    $i++;
}
print "\n";



# create a "binary" file;
# test with:
# $ hexdump file.bin 

open OUT, ">", "file.bin"
    or die $!;
for (0..255) {
    print OUT chr($_);
}
close OUT;



# create "binary" files with different encodings;
#
# test with:
# $ hexdump file.bin 
# $ hexdump utf8.bin 

open OUT, ">", "file.bin"
    or die $!;
for (0..255) {
    print OUT chr($_);
}
close OUT;

open OUT, "> :encoding(UTF-8)", "utf8.bin"
    or die $!;
for (0..255) {
    print OUT chr($_);
}
close OUT;


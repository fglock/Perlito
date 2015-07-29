my $count;
my $s = "a";
my $i = 0;
while ( $i < 400 ) {
    my $j = 0;
    while ( $j < 400 ) {
        my $k = 0;
        while ( $k < 400 ) {
            $k++;
            $s++;
            $count++;
        }
        $j++;
    }
    $i++;
}

print "done $count $s\n";

